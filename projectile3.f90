program projectile_motion
implicit none

integer::i
real::v,theta,vx,vy,vx0,vy0,x0,y0,t,theta1,vx1,vy1,x1,y1,theta2,vx2,vy2,x2,y2
real,parameter::pi=acos(-1.0)
real,dimension(1:1000)::x,y
print*,"give v and theta and theta1"
read*,v,theta,theta1,theta2

x0=0
y0=0

vx0=v*cos(pi*theta/180)
vy0=v*sin(pi*theta/180)

vx1=v*cos(pi*theta1/180)
vy1=v*sin(pi*theta1/180)

vx2=v*cos(pi*theta2/180)
vy2=v*sin(pi*theta2/180)

open(1,file="projectile.dat")
t=0
do i=0,1000
x(i)=vx0*t+x0
y(i)=vy0*t-0.5*9.8*t**2+y0

x1=vx1*t+x0
y1=vy1*t-0.5*9.8*t**2+y0

x2=vx2*t+x0
y2=vy2*t-0.5*9.8*t**2+y0

if(mod(i,2)==0)then
write(1,*)x(i),y(i),x1,y1,x2,y2
end if

t=t+0.1
end do

end program
