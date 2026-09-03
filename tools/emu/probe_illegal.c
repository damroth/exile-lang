/* Negative control 1, fed to the machine on every run of the gate: 0x4AFC is the
 * 68000's ILLEGAL instruction. An emulator that shrugged at an opcode - took the
 * exception quietly, or treated an unrecognised word as a no-op - would turn a
 * miscompilation into a green run, so this probe must STOP the machine. */
int main(void) { __asm__ __volatile__(".short 0x4afc"); return 0; }
