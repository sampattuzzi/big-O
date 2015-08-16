use "analysis.sml";

val (testBeta, testR2) = linRegMat (RealMatrix.fromList [[2.0,1.0],[3.0,1.0],[4.0,1.0]])
(RealMatrix.fromList [[1.0],[2.0],[3.0]]);

val (testBeta, testR2) = linRegMat (RealMatrix.fromList [[1.0],[1.0],[1.0]])
(RealMatrix.fromList [[2.0],[2.0],[2.0]]);

val (testBeta, testR2) = linRegMat (RealMatrix.fromList [[2.0, 1.0],[4.0,
1.0],[8.0, 1.0]])
(RealMatrix.fromList [[1.0],[2.0],[4.0]]);

val (testBeta, testR2) = linRegMat (RealMatrix.fromList [[2.0, 1.0],[4.0,
1.0],[8.0, 1.0]])
(RealMatrix.fromList [[1.0],[2.0],[3.0]]);

