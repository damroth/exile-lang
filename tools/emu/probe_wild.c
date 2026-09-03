/* Negative control 2, fed to the machine on every run of the gate: a read from an
 * address the machine does not map. An emulator that answered zero for unmapped
 * memory would let a wild pointer read plausible data and keep going - which is
 * exactly the fault a real machine gave us once, and this one must not be gentler
 * than that. */
int main(void)
{
    volatile unsigned char *p = (volatile unsigned char *)0x00A00000UL;
    return (int)*p;
}
