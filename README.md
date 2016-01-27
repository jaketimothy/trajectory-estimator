# trajectory-estimator

This project is an effort to bring some key navigation algorithms, e.g. ODE integration, into the distributed computing world through an implementation with Apache Spark at its core. Initial iterations of this project will implement basic (local) scala methods to create an overall architecture. These will be replaced along the way with parallelized computations, culminating in a deeper investigation into Spark usage and any applicable optimizations.

The work hourse algorithm in this case is the [unscented Kalman filter](https://www.seas.harvard.edu/courses/cs281/papers/unscented.pdf) (UKF).  @rlabbe has a great [book on filters](https://github.com/rlabbe/Kalman-and-Bayesian-Filters-in-Python) for learning more about the UKF and others.  Much of the burden of filtering a nonlinear system falls on the ODE integrator used. The implementation of a scala ODE integrator will be in the form of a contribution to scalanlp.breeze.

The spherical harmonic model of Earth's gravity is used and the gravity model implementation makes extensive use of B. A. Jones' [PHD thesis](http://ccar.colorado.edu/geryon/papers/Misc/bajones_phd.pdf).
