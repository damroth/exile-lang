/* The chipset machine: a 68000 executing our own bare-metal witnesses, with a
 * custom-register window we model ourselves.
 *
 * Why it exists. Everything this repository says about the Amiga is a statement
 * about EMISSION - the store is at this offset, the C is clean, the link closes.
 * Nothing has ever been a statement about BEHAVIOUR. The NDK declares four ward
 * instances at the real 0xDFF000 and has no consumer for any of them, because a
 * consumer would fault on the host. This is the tool that gives them one.
 *
 * What it is NOT. There is no Kickstart, no exec, no library base, no disk. The
 * witnesses are freestanding: they link `-nostdlib`, they start at `_main`, and
 * their whole contact with the world is a serial register and a status register.
 * Modelling an Amiga would mean modelling AmigaOS, which is the thing this
 * project spent the arc refusing to bind to.
 *
 * The rule this file is written under. A tool that verifies a compiler must be
 * STRICTER than the thing it replaces, never gentler: an emulator that maps all
 * of memory lets a wild pointer read zeroes and go green, which is a language bug
 * arriving disguised as a pass. So the map is small and everything outside it is
 * a hard stop, and an instruction the CPU does not recognise is a hard stop too.
 * Both are proved by feeding the machine one, in the gate, every run.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "musashi/m68k.h"
#include "emuhooks.h"

/* ---- the memory map ---------------------------------------------------- */

#define RAM_SIZE     0x00200000UL          /* 2 MB of chip RAM */
#define CUSTOM_BASE  0x00DFF000UL
#define CUSTOM_END   0x00DFF200UL
#define HALT_PC      0x00000100UL          /* below the load base, above the vectors */

/* The serial registers the bare seam actually uses - and only those. A register
 * this machine does not model is not silently zero: it is a stop, because a
 * driver reading a register nobody implemented would otherwise get a plausible
 * answer and keep going. */
#define SERDATR      0x018
#define SERDAT       0x030
#define SERPER       0x032
#define SERDATR_TBE  0x2000

/* The SET/CLR pairs. Bit 15 of a write says whether the other bits are SET or
 * CLEARED in the live register, and the live value is readable only from a
 * separate port - which is why the library declares each pair atomic and why a
 * driver that touches one has to seal.
 *
 * Modelling these as plain registers would be the quiet kind of wrong: a driver
 * clearing one bit would appear to zero every other bit, and a gate watching the
 * read port would confirm it. The whole point of the read port is that it answers
 * with STATE, not with the last word written. */
#define DMACONR      0x002
#define INTENAR      0x01C
#define INTREQR      0x01E
#define DMACON       0x096
#define INTENA       0x09A
#define INTREQ       0x09C

/* The coprocessor. Its registers are ordinary; what is not ordinary is that it
 * FETCHES from the program's own memory and writes chip registers with no CPU
 * store anywhere in sight - which is the whole observation this increment exists
 * to make.
 *
 * Three things here are a MODEL and not a reproduction, and saying so is cheaper
 * than having someone discover it:
 *   - the beam is a COUNTER. It advances one step per unsatisfied wait, so a list
 *     finishes; it does not track cycles, lines or a raster.
 *   - concurrency is INTERLEAVING. The copper takes one step every
 *     COP_STEP_EVERY CPU instructions, not a DMA slot in a cycle-accurate frame.
 *   - END is recognised explicitly. On the chip, `$FFFF,$FFFE` is a wait nobody
 *     reaches until the frame restarts; this machine has no frame, so the list
 *     stops there instead of blocking forever.
 * What is NOT modelled loosely is the gate on the whole thing: the copper fetches
 * only while its own DMA bit and the master bit are both set, because that is the
 * switch this increment's floor turns off. */
#define COPCON       0x02E
#define COP1LCH      0x080
#define COP1LCL      0x082
#define COPJMP1      0x088
#define DMAF_COP     0x0080
#define DMAF_MAST    0x0200
#define COP_STEP_EVERY 4u

/* Paula's interrupt controller, reduced to the one source this increment needs.
 * INTREQ holds what has happened, INTENA what is allowed through, and bit 14 of
 * INTENA is the master switch; a source passes only when all three agree. VBLANK
 * is bit 5 and arrives on the 68000 as level 3, which the CPU takes through the
 * AUTOVECTOR at 0x6C - no vector fetch from the chip, which is why the stub can
 * install itself by writing one longword.
 *
 * The frame is INJECTED: this machine has no raster, so VBLANK is raised every
 * VBLANK_EVERY instructions. That is a model of WHEN, not of whether - the gating
 * above is the real thing, and it is what the floor turns off. */
#define VBLANK_BIT   0x0020
#define INT_MASTER   0x4000
#define VBLANK_LEVEL 3
#define VBLANK_EVERY 20000u

static unsigned char ram[RAM_SIZE];
static unsigned short dmacon, intena, intreq;
static unsigned int   cop1lc, cop_pc, cop_beam;
static unsigned long  frames;
static int            cop_running;
static unsigned int  load_base, load_end, bss_base, bss_end;
static int           halted;
static unsigned long insns, insn_budget = 20000000UL;

static void fault(const char *what, unsigned int addr)
{
    unsigned int pc = m68k_get_reg(NULL, M68K_REG_PPC);
    fflush(stdout);
    fprintf(stderr, "emu: %s at 0x%06x (PC=0x%06x)\n", what, addr, pc);
    fprintf(stderr, "emu: the map is RAM 0x000000-0x%06lx and custom 0x%06lx-0x%06lx;"
                    " nothing else answers, on purpose\n",
            (unsigned long)RAM_SIZE - 1UL, CUSTOM_BASE, CUSTOM_END - 1UL);
    exit(3);
}

/* ---- negative control 1: an instruction the CPU does not recognise ------ */

static int on_illegal(int opcode)
{
    unsigned int pc = m68k_get_reg(NULL, M68K_REG_PPC);
    fflush(stdout);
    fprintf(stderr, "emu: illegal instruction 0x%04x at PC=0x%06x\n",
            (unsigned int)opcode & 0xffffu, pc);
    fprintf(stderr, "emu: stopping rather than taking the exception - an emulator that"
                    " shrugs at an opcode turns a miscompilation into a green run\n");
    exit(4);
    return 0;
}

/* ---- the custom chip --------------------------------------------------- */

static void custom_write16(unsigned int off, unsigned int val);

/* The interrupt line is LEVEL-triggered, and getting that wrong is the softest
 * possible way to invent hardware. Configured for its default acknowledge scheme
 * the core clears the level the moment the exception is taken, which models an
 * EDGE: a handler that forgets to acknowledge its source finishes normally, the
 * gate goes green, and the same driver livelocks on the chip - where INTREQ still
 * holds the bit, the enable and the master still stand, and the line is therefore
 * still asserted when `rte` restores the mask.
 *
 * So the machine takes over the acknowledge and does NOT clear anything. The level
 * stays exactly what (INTREQ, INTENA, master) says it is, re-evaluated after every
 * write to either register - which is what makes acknowledging the source the
 * thing that lowers the line, as on the chip. */
static int on_int_ack(int level)
{
    (void)level;
    return M68K_INT_ACK_AUTOVECTOR;
}

/* A source is asserted only when it has HAPPENED, is ENABLED, and the master
 * switch is on. */
static void irq_refresh(void)
{
    unsigned int pend = (unsigned int)intreq & (unsigned int)intena;
    if ((intena & INT_MASTER) != 0 && (pend & VBLANK_BIT) != 0) m68k_set_irq(VBLANK_LEVEL);
    else m68k_set_irq(0);
}

/* Bit 15 decides the direction; the remaining bits are the mask it applies. */
static unsigned short setclr(unsigned short cur, unsigned int val)
{
    unsigned short bits = (unsigned short)(val & 0x7fffu);
    if (val & 0x8000u) return (unsigned short)(cur | bits);
    return (unsigned short)(cur & (unsigned short)~bits);
}

static unsigned int custom_read16(unsigned int off)
{
    if (off == SERDATR) return SERDATR_TBE;   /* transmit buffer always empty */
    if (off == DMACONR) return dmacon;
    if (off == INTENAR) return intena;
    if (off == INTREQR) return intreq;
    fault("read from an unmodelled custom register", (unsigned int)CUSTOM_BASE + off);
    return 0;
}

/* One copper instruction. Reads go through the ordinary bus, so a list that walks
 * off the map faults exactly as a CPU access would - the coprocessor does not get
 * a gentler machine than the processor. */
static void copper_step(void)
{
    unsigned int w0, w1;
    if (!cop_running) return;
    if ((dmacon & DMAF_COP) == 0 || (dmacon & DMAF_MAST) == 0) return;
    w0 = m68k_read_memory_16(cop_pc);
    w1 = m68k_read_memory_16(cop_pc + 2u);
    if ((w0 & 1u) == 0u) {                       /* MOVE: (register offset, value) */
        cop_pc += 4u;
        custom_write16(w0 & 0x1feu, w1);
        return;
    }
    if ((w1 & 1u) == 0u) {                       /* WAIT */
        if (w0 == 0xffffu && w1 == 0xfffeu) { cop_running = 0; return; }
        if (cop_beam >= (w0 >> 8)) cop_pc += 4u; else cop_beam++;
        return;
    }
    cop_pc += 4u;                                /* SKIP: not exercised yet */
}

static void custom_write16(unsigned int off, unsigned int val)
{
    if (off == SERDAT) { putchar((int)(val & 0xffu)); return; }
    if (off == SERPER) return;                /* baud divisor: accepted, unmodelled */
    if (off == DMACON) { dmacon = setclr(dmacon, val); return; }
    if (off == COP1LCH) { cop1lc = (cop1lc & 0x0000ffffu) | ((val & 0xffffu) << 16); return; }
    if (off == COP1LCL) { cop1lc = (cop1lc & 0xffff0000u) | (val & 0xffffu); return; }
    if (off == COPJMP1) { cop_pc = cop1lc; cop_beam = 0u; cop_running = 1; return; }
    if (off == COPCON) return;                /* copper danger bit: accepted, unmodelled */
    if (off == INTENA) { intena = setclr(intena, val); irq_refresh(); return; }
    if (off == INTREQ) { intreq = setclr(intreq, val); irq_refresh(); return; }
    fault("write to an unmodelled custom register", (unsigned int)CUSTOM_BASE + off);
}

/* ---- the bus ----------------------------------------------------------- */

static int in_ram(unsigned int a, unsigned int n) { return (unsigned long)a + n <= RAM_SIZE; }
static int in_custom(unsigned int a) { return a >= CUSTOM_BASE && a < CUSTOM_END; }

unsigned int m68k_read_memory_8(unsigned int a)
{
    if (in_ram(a, 1)) return ram[a];
    if (in_custom(a)) return (custom_read16((a & ~1u) - CUSTOM_BASE) >> ((a & 1u) ? 0 : 8)) & 0xffu;
    fault("byte read outside the map", a);
    return 0;
}
unsigned int m68k_read_memory_16(unsigned int a)
{
    if (in_ram(a, 2)) return ((unsigned int)ram[a] << 8) | ram[a + 1];
    if (in_custom(a)) return custom_read16(a - CUSTOM_BASE);
    fault("word read outside the map", a);
    return 0;
}
unsigned int m68k_read_memory_32(unsigned int a)
{
    if (in_ram(a, 4))
        return ((unsigned int)ram[a] << 24) | ((unsigned int)ram[a+1] << 16)
             | ((unsigned int)ram[a+2] << 8) | ram[a+3];
    if (in_custom(a)) return (custom_read16(a - CUSTOM_BASE) << 16)
                           | custom_read16(a + 2 - CUSTOM_BASE);
    fault("long read outside the map", a);
    return 0;
}
void m68k_write_memory_8(unsigned int a, unsigned int v)
{
    if (in_ram(a, 1)) { ram[a] = (unsigned char)v; return; }
    if (in_custom(a)) { custom_write16((a & ~1u) - CUSTOM_BASE, v & 0xffu); return; }
    fault("byte write outside the map", a);
}
void m68k_write_memory_16(unsigned int a, unsigned int v)
{
    if (in_ram(a, 2)) { ram[a] = (unsigned char)(v >> 8); ram[a+1] = (unsigned char)v; return; }
    if (in_custom(a)) { custom_write16(a - CUSTOM_BASE, v & 0xffffu); return; }
    fault("word write outside the map", a);
}
void m68k_write_memory_32(unsigned int a, unsigned int v)
{
    if (in_ram(a, 4)) {
        ram[a] = (unsigned char)(v >> 24); ram[a+1] = (unsigned char)(v >> 16);
        ram[a+2] = (unsigned char)(v >> 8); ram[a+3] = (unsigned char)v; return;
    }
    if (in_custom(a)) { custom_write16(a - CUSTOM_BASE, (v >> 16) & 0xffffu);
                        custom_write16(a + 2 - CUSTOM_BASE, v & 0xffffu); return; }
    fault("long write outside the map", a);
}

void emu_instr_hook(unsigned int pc)
{
    if (pc == (unsigned int)HALT_PC) { halted = 1; m68k_end_timeslice(); return; }
    if ((insns % COP_STEP_EVERY) == 0u) copper_step();
    if ((insns % (unsigned long)VBLANK_EVERY) == 0u) {
        frames++;
        intreq = (unsigned short)(intreq | VBLANK_BIT);
        irq_refresh();
    }
    if (++insns > insn_budget) {
        fflush(stdout);
        fprintf(stderr, "emu: instruction budget exhausted at PC=0x%06x - the program"
                        " is not going to finish, and a hang is not a result\n", pc);
        exit(5);
    }
}

/* ---- the loader: an AmigaOS hunk, relocated where we choose ------------- */

#define HUNK_CODE 0x3e9u
#define HUNK_DATA 0x3eau
#define HUNK_BSS  0x3ebu
#define HUNK_RELOC32 0x3ecu
#define HUNK_SYMBOL  0x3f0u
#define HUNK_DEBUG   0x3f1u
#define HUNK_END     0x3f2u
#define HUNK_HEADER  0x3f3u

static unsigned char *img;
static long img_len;
static long ip;

static unsigned int be32(void)
{
    unsigned int v;
    if (ip + 4 > img_len) { fprintf(stderr, "emu: the executable ends mid-hunk\n"); exit(2); }
    v = ((unsigned int)img[ip] << 24) | ((unsigned int)img[ip+1] << 16)
      | ((unsigned int)img[ip+2] << 8) | img[ip+3];
    ip += 4;
    return v;
}

/* Two hunks is what our witnesses are: code and bss. Enough is enough - a loader
 * that guessed at shapes it has never seen would be inventing behaviour. */
static unsigned int hunk_addr[2];
static unsigned int entry;

static void load(const char *path)
{
    FILE *f = fopen(path, "rb");
    unsigned int t, i, n, cnt, first, last, sizes[2], place;
    sizes[0] = sizes[1] = 0u;
    int cur = -1;
    if (!f) { fprintf(stderr, "emu: cannot open %s\n", path); exit(2); }
    fseek(f, 0, SEEK_END); img_len = ftell(f); fseek(f, 0, SEEK_SET);
    img = (unsigned char *)malloc((size_t)img_len);
    if (!img || fread(img, 1, (size_t)img_len, f) != (size_t)img_len) {
        fprintf(stderr, "emu: cannot read %s\n", path); exit(2);
    }
    fclose(f);

    if (be32() != HUNK_HEADER) { fprintf(stderr, "emu: not a hunk executable\n"); exit(2); }
    while ((n = be32()) != 0) ip += (long)n * 4;   /* resident library names */
    cnt = be32(); first = be32(); last = be32();
    if (cnt > 2 || first != 0 || last + 1u != cnt) {
        fprintf(stderr, "emu: this loader models one or two hunks (code, and a bss if there"
                        " is one); got %u hunks %u..%u\n", cnt, first, last);
        exit(2);
    }
    for (i = 0; i < cnt; i++) sizes[i] = be32() & 0x3fffffffu;

    place = 0x1000;                                /* clear of the vectors */
    for (i = 0; i < cnt; i++) {
        hunk_addr[i] = place;
        place = (place + sizes[i] * 4u + 15u) & ~15u;
    }
    if (place >= RAM_SIZE) { fprintf(stderr, "emu: the image does not fit in RAM\n"); exit(2); }
    load_base = hunk_addr[0]; load_end = hunk_addr[0] + sizes[0] * 4u;
    bss_base  = cnt > 1 ? hunk_addr[1] : 0u;
    bss_end   = cnt > 1 ? hunk_addr[1] + sizes[1] * 4u : 0u;

    while (ip < img_len) {
        t = be32() & 0x3fffffffu;
        if (t == HUNK_CODE || t == HUNK_DATA) {
            cur++;
            n = be32();
            if (cur > 1) { fprintf(stderr, "emu: more loadable hunks than the header promised\n"); exit(2); }
            memcpy(ram + hunk_addr[cur], img + ip, (size_t)n * 4);
            ip += (long)n * 4;
        } else if (t == HUNK_BSS) {
            cur++;
            n = be32();
            if (cur > 1) { fprintf(stderr, "emu: more hunks than the header promised\n"); exit(2); }
            memset(ram + hunk_addr[cur], 0, (size_t)n * 4);
        } else if (t == HUNK_RELOC32) {
            while ((n = be32()) != 0) {
                unsigned int target = be32();
                if (target > 1 || cur < 0) { fprintf(stderr, "emu: relocation names an unknown hunk\n"); exit(2); }
                for (i = 0; i < n; i++) {
                    unsigned int off = be32();
                    unsigned int at = hunk_addr[cur] + off;
                    unsigned int v = m68k_read_memory_32(at);
                    m68k_write_memory_32(at, v + hunk_addr[target]);
                }
            }
        } else if (t == HUNK_SYMBOL) {
            while ((n = be32()) != 0) {
                char name[64];
                unsigned int len = n * 4u, val;
                unsigned int take = len < sizeof(name) - 1u ? len : (unsigned int)sizeof(name) - 1u;
                memcpy(name, img + ip, take); name[take] = 0;
                ip += (long)len;
                val = be32();
                if (cur >= 0 && strcmp(name, "_main") == 0) entry = hunk_addr[cur] + val;
            }
        } else if (t == HUNK_DEBUG) {
            n = be32(); ip += (long)n * 4;
        } else if (t == HUNK_END) {
            /* nothing */
        } else {
            fprintf(stderr, "emu: hunk type 0x%03x is not one this loader models\n", t);
            exit(2);
        }
    }
    if (!entry) {
        fprintf(stderr, "emu: no `_main` in the symbol table - this loader starts at the"
                        " symbol, not at hunk offset zero, because the linker does not put"
                        " the entry first\n");
        exit(2);
    }
}

int main(int argc, char **argv)
{
    unsigned int sp = RAM_SIZE - 16u;
    if (argc < 2) { fprintf(stderr, "usage: emu <hunk-executable>\n"); return 2; }
    load(argv[1]);

    /* Reset vectors, then the return address `_main` will eventually take: a
     * sentinel the instruction hook recognises, so a finished program stops
     * instead of running into whatever follows it. */
    m68k_write_memory_32(0, sp);
    m68k_write_memory_32(4, entry);
    m68k_init();
    m68k_set_cpu_type(M68K_CPU_TYPE_68000);
    m68k_set_illg_instr_callback(on_illegal);
    m68k_set_int_ack_callback(on_int_ack);
    m68k_pulse_reset();
    sp -= 4;
    m68k_write_memory_32(sp, (unsigned int)HALT_PC);
    m68k_set_reg(M68K_REG_SP, sp);
    m68k_set_reg(M68K_REG_PC, entry);

    while (!halted) m68k_execute(100000);
    fflush(stdout);
    return 0;
}
