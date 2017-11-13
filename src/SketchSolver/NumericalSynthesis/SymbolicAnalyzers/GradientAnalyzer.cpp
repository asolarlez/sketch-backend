#include "GradientAnalyzer.h"
#include <algorithm>
#include <limits>
#include <math.h>

void GradientAnalyzer::visit( SRC_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( DST_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( ASSERT_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( CTRL_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( PLUS_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " + " << eval->getVal(node.father) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.father));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << " " << gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << " " << gf << endl;
        }
    }
}

void GradientAnalyzer::visit( TIMES_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " * " << eval->getVal(node.father) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.father));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << " " << gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << " " << gf << endl;
        }
    }

}

void GradientAnalyzer::visit(ARRACC_node& node )  {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " > 0 $ " << eval->getVal(node.multi_mother[0]) << ", " << eval->getVal(node.multi_mother[1]) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gc = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.multi_mother[0]));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.multi_mother[1]));
    if (printGrads) {
        cout << "Grads: " << g << " " << gc << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold || gc >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gc << " " << gm << " "<< gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold || gc <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gc << " " << gm << " " << gf << endl;
        }
    }

}

void GradientAnalyzer::visit( DIV_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " / " << eval->getVal(node.father) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.father));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << " " << gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << " " << gf << endl;
        }
    }

}

void GradientAnalyzer::visit( MOD_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( NEG_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = -" << eval->getVal(node.mother) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << endl;
    }
    if(printGradLoss) {
        if ((gm >= threshold ) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm  << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << endl;
        }
    }

}

void GradientAnalyzer::visit( CONST_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( LT_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " < " << eval->getVal(node.father) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.father));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << " " << gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << " " << gf << endl;
        }
    }

    
}

void GradientAnalyzer::visit( EQ_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( AND_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " && " << eval->getVal(node.father) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.father));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << " " << gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << " " << gf << endl;
        }
    }

    
}

void GradientAnalyzer::visit( OR_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = " << eval->getVal(node.mother) << " || " << eval->getVal(node.father) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    double gf = gsl_blas_dnrm2(eval->getGrad(node.father));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << " " << gf << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold || gf >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << " " << gf << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold || gf <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << " " << gf << endl;
        }
    }

}

void GradientAnalyzer::visit( NOT_node& node ) {
    if (printVals) {
        cout << eval->getVal(&node) << " = !" << eval->getVal(node.mother) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(node.mother));
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << endl;
        }
    }

    
}

void GradientAnalyzer::visit( ARRASS_node& node ) {
    // Ignore
}

void GradientAnalyzer::visit( UFUN_node& node ) {
    const string& name = node.get_ufname();
    bool_node* mother = node.multi_mother[0];
    if (printVals) {
        cout << eval->getVal(&node) << " = " << name << " " << eval->getVal(mother) << endl;
    }
    double g = gsl_blas_dnrm2(eval->getGrad(&node));
    double gm = gsl_blas_dnrm2(eval->getGrad(mother));
    
    if (printGrads) {
        cout << "Grads: " << g << " " << gm << endl;
    }
    if (printGradLoss) {
        if ((gm >= threshold) && g <= threshold) {
            cout << "GRADIENT LOSS " << g << " " << gm << endl;
        }
    }
    if (printGradBlowup) {
        if ((gm <= upThreshold) && g >= upThreshold) {
            cout << "GRADIENT BLOWUP " << g << " " << gm << endl;
        }
    }

}

void GradientAnalyzer::visit( TUPLE_R_node& node) {
    // Ignore
}

void GradientAnalyzer::run() {
    for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
        cout << (*node_it)->lprint() << endl;
        (*node_it)->accept(*this);
        //cout << gsl_vector_get(eval->getGrad(*node_it), 2) << endl;
    }
}

void GradientAnalyzer::run(bool_node* n) {
    set<int> ids = Util::getRelevantNodes(n);
    for (auto it = ids.begin(); it != ids.end(); it++) {
        bool_node* node = bdag[*it];
        cout << node->lprint() << endl;
        node->accept(*this);
    }
}
