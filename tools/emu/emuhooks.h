/* Declarations the vendored core needs when it is configured, from the compiler
 * command line, to call back into this machine. Kept here so the core's own tree
 * stays byte-identical to upstream: every option it exposes is guarded by
 * `#ifndef`, so the configuration is ours without a single edit of theirs. */
#ifndef EMUHOOKS_H
#define EMUHOOKS_H
void emu_instr_hook(unsigned int pc);
#endif
