#include "driver.h"

void runDriver() {
	PyDriver m(*PARAMS);
	m.parseInput();
}

InterpreterEnvironment* getEnvt() {
	return INp::envt;
}
