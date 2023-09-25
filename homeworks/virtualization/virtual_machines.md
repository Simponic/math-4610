* Elizabeth Hunt (A02364151), MATH 4610

## Virtual Machines

**Question 1**

Run the Linux OS as a virtual machine, or run the application in a containerized Linux environment (which
is the same abstraction).

**Question 2**

A native system virtual machine has dedicated hardware to run the hypervisor, while a hosted system 
virtual machine runs a hypervisor as a process in the operating system.

**Question 3**

A virtual machine hosts an entire operating system and requires users to perform configuration if they
want to run an application, whereas a Virtual Appliance is built to provide an easy plug-and-play virtual 
machine image built to run some specific software stack.

**Question 4**

In a large application sense, containerizing services into their own virtual machines allows for easier
replication, scaling, and networking. Instead of running several smaller servers, one large server can
host several applications in parallel. This provides a good seperation of concern. And, if one service 
goes down, the whole system does not go down with it.

Locally, it can help in development when targeting another operating system. Virtual machines can be
used to verify builds without installing a whole other operating system.

**Question 5**

A virtual machine monitor is just another term for a hypervisor, so, see question 2.

**Question 6**

The three components of a virtual machine are:

1. The host
2. The virtualization layer
3. The guest
