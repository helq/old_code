#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import png
from math import pi, sin, cos, acos, atan, sqrt

def normalize_LatLon(lat, lon):
    if lat < -pi/2 or lat > pi/2:
        lat = (lat + pi)%(2*pi) - pi
        if lat < -pi/2:
            lat  = - pi - lat
            lon += pi
        if lat > pi/2:
            lat  = pi - lat
            lon += pi

    return (lat, (lon + pi)%(2*pi) - pi )

def cartesianTo_LatLon(v):
    [x,y,z] = v
    theta = acos(z/sqrt(x**2+y**2+z**2))
    lon = atan(y/x) + (0 if x>0 else pi) if x != 0 else pi/2
    # spherical to lat-lon system
    lat = pi/2 - theta
    return (lat, lon)

def equirectangularProjection(data, n, m):
    """ return a function that return the value from `data' (a equirectangular
        projection) given an específic latitude and longitude (in radians).

        Equirectangular Projection also called `Plate Carrée'
    """
    def location(latitude, longitude):
        (lat,lon) = normalize_LatLon(latitude, longitude)
        x = (lat+(pi/2))/(pi/2) *(m/2)
        y =     (lon+pi)/pi     *(n/2)
        x = min(m-1, x)
        y = min(n-1, y)
        return data(int(x), int(y))
    return location

def planeProjection(lat, lon, scale):
    # from latitud-longitud system to spherical coordinate system
    # r     = 1
    # theta = pi/2 - lat
    # phi   = lon
    lat = -lat # the nord latitudes are positive

    # vector 1 (in lat-lon system)
    x1 = cos(lat) * cos(lon)
    y1 = cos(lat) * sin(lon)
    z1 = sin(lat)
    #v1 = [x1, y1, z1]
    # vector 2
    x2 = cos(lon + pi/2)
    y2 = sin(lon + pi/2)
    z2 = 0
    #v2 = [x2, y2, z2]
    # vector 3
    x3 = cos(lat + pi/2) * cos(lon)
    y3 = cos(lat + pi/2) * sin(lon)
    z3 = sin(lat + pi/2)
    #v3 = [x3, y3, z3]

    def f(n, m):
        #v = v1 + (m*v2 + n*v3) * scale
        x4 = x1 + (m*x2 + n*x3) * scale
        y4 = y1 + (m*y2 + n*y3) * scale
        z4 = z1 + (m*z2 + n*z3) * scale
        return cartesianTo_LatLon([x4,y4,z4])
    return f

# reading equirectangular projection map
fmap = png.Reader( file=open('gebco_08_rev_elev_21600x10800.png','r') )
#fmap = png.Reader( file=open('world.topo.bathy.200412.3x5400x2700.png','r') )
#fmap = png.Reader( file=open('Equirectangular_projection_SW.png','r') )

print "reading image ...",
image = fmap.read()
print "converting image in a readable list ..."
n, m, rgb = image[0], image[1], list(image[2])

map_rgb = lambda j,i: (rgb[j][3*i],  rgb[j][3*i+1],  rgb[j][3*i+2])

# location

## Germany
#latitude = 52.378601
#longitude = 9.6933306
#zoom=1.5

## Colombia
#latitude = 3.7034233
#longitude = -73.579901
#zoom=1.5

# Antartic
latitude = -90
longitude = -73
zoom=0.8

## Africa
#latitude = 0
#longitude = 14
#zoom=0.003

##
#latitude = 73
#longitude = -40
#zoom=1.0

# creating map using basicProjection
newMap = []
n_ = 600
m_ = 700

getPixel = equirectangularProjection( map_rgb, n, m )
basicProjection = planeProjection( latitude*pi/180
                                 , longitude*pi/180
                                 , 1/float(zoom*min(n_, m_))
                                 )

print "constructing new image ..."
for i in range(n_):
    newMap.append([])
    for j in range(m_):
        (lat, lon) = basicProjection(i-(n_/2), j-(m_/2))
        (r,g,b) = getPixel(lat, lon)
        newMap[-1] += [r,g,b]

f = open('newMap.png', 'wb')
w = png.Writer(m_, n_)
w.write(f, newMap)
f.close()
