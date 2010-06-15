#include "driver.h"

void runDriver() {
	Driver m(*PARAMS);
	m.parseInput();
}

InterpreterEnvironment* getEnvt() {
	return INp::envt;
}
