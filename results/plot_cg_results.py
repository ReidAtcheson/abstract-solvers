import numpy as np
import matplotlib.pyplot as plt
plt.ioff()

for maxit in range(1,11):
    fname = "abstract_cg_{}.dat".format(maxit)
    fname_dom = "abstract_cg_domain.dat"
    ys = np.loadtxt(fname)
    xs = np.loadtxt(fname_dom)
    fig=plt.figure()
    os=np.ones(xs.size)
    ymin=0
    ymax=2
    plt.plot(xs,ys)
    plt.plot(xs,os)
    axes=plt.gca()
    axes.set_ylim([ymin,ymax])
    fig.savefig("abstract_cg_{}.pdf".format(maxit))


for maxit in range(1,11):
    fname = "one_cg_{}.dat".format(maxit)
    fname_dom = "one_cg_domain.dat"
    ys = np.loadtxt(fname)
    xs = np.loadtxt(fname_dom)
    fig=plt.figure()
    os=np.ones(xs.size)
    ymin=0
    ymax=2
    plt.plot(xs,ys)
    plt.plot(xs,os)
    axes=plt.gca()
    axes.set_ylim([ymin,ymax])
    fig.savefig("one_cg_{}.pdf".format(maxit))


