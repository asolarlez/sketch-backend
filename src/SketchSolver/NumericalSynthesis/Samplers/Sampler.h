#pragma once
class Sampler {
public:
    virtual void sampleState(gsl_vector* state) = 0;
};
