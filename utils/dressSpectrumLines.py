#!/usr/bin/python
# -*- coding: utf-8 -*-
############################################################################
#    Copyright (C) 2015 by Nestor Aguirre                                  #
#    nfaguirrec@gmail.com                                                  #
#                                                                          #
#    This program is free software; you can redistribute it and#or modify  #
#    it under the terms of the GNU General Public License as published by  #
#    the Free Software Foundation; either version 2 of the License, or     #
#    (at your option) any later version.                                   #
#                                                                          #
#    This program is distributed in the hope that it will be useful,       #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#    GNU General Public License for more details.                          #
#                                                                          #
#    You should have received a copy of the GNU General Public License     #
#    along with this program; if not, write to the                         #
#    Free Software Foundation, Inc.,                                       #
#    59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             #
############################################################################

import sys
import math
import numpy

class NFunction:
        def __init__( this, xRange, nPoints ):
		this.x = numpy.linspace( xRange[0], xRange[1], nPoints, endpoint=True )
		this.y = numpy.zeros( nPoints )
		
	def __str__( this ):
		output = "#%19s"%"x" + "%20s\n"%"y"
		output += "#%19s"%"-------" + "%20s\n"%"-------"
		
		for i in range( 0, len(this.x) ):
			output += "%20.10f"%this.x[i] + "%20.10f\n"%this.y[i]

		return output

class Absorption:
        def __init__( this, energy, intensity ):
                this.energy = energy
                this.intensity = intensity

class Spectrum:
	
	def __init__( this, energyRange, nPoints, profile, width ):
		this.energyRange = energyRange
		this.nPoints = nPoints
		this.profile = profile
		this.width = width
		
		this.absorptionsList = []
		this.spectrumData = NFunction( this.energyRange, this.nPoints )
	
	def loadAbsorptions( this, inputFileName, column ):
		del this.absorptionsList[:]
		
		ifile = file(inputFileName, 'r')
			
		i=0
		for line in ifile:
			if( len(line) <= 1 ):
				continue
			
			tokens = line.split()
			
			if( len(tokens) > 1 ):
				this.absorptionsList.append( Absorption( float(tokens[0]), float(tokens[column-1]) ) )
			else:
				this.absorptionsList.append( Absorption( float(tokens[0]), 1.0 ) )
			
			i+=1
			
		ifile.close()
		
	def buildProfile( this ):
		this.spectrumData = NFunction( this.energyRange, this.nPoints )
		
		if( this.profile == "GAUSSIAN" ):
			for i in range( 0, len(this.spectrumData.x) ):
				for abso in this.absorptionsList:
					this.spectrumData.y[i] = this.spectrumData.y[i] \
					+ this.GaussianPeak( this.spectrumData.x[i], abso.energy, abso.intensity, this.width )
		elif( this.profile == "LORENTZIAN" ):
			for i in range( 0, len(this.spectrumData.x) ):
				for abso in this.absorptionsList:
					this.spectrumData.y[i] = this.spectrumData.y[i] \
					+ this.LorentzianPeak( this.spectrumData.x[i], abso.energy, abso.intensity, this.width )
		else:
			print "### ERROR ### profile = ", this.profile, " is not implemented. Possible values are GAUSSIAN or LORENTZIAN."
			exit(-1)
		
	def GaussianPeak( this, E, energy, intensity, FWHM ):
		
		width = FWHM/2.35482
		output = intensity*math.exp( -(E-energy)**2/2.0/width**2 )
		
		return output
		
	def LorentzianPeak( this, E, energy, intensity, FWHM ):
		
		width = 0.5*FWHM
		output = intensity/( 1.0+((E-energy)/width)**2 )
		
		return output

def main():
	# ----------------
	# Test in gnuplot
	# ----------------
	# unset key; set size 1.0,0.5; plot [4000:500] [600:0] "< ./dressSpectrumLines.py IRspectrum.dat gaussian 10.0 500.0 4000.0 1000 2" w l lw 2
	
	if( len( sys.argv ) == 8 ):
		fileName = sys.argv[1]
		profile = sys.argv[2]
		width = float(sys.argv[3])
		
		energyRange = [ 0.0, 0.0 ]
		energyRange[0] = float(sys.argv[4])
		energyRange[1] = float(sys.argv[5])
		nPoints = int(sys.argv[6])
		
		column = int(sys.argv[7])
	else:
		print "usage:"
		print "    $ dressSpectrumLines.py fileName profile width minE maxE nPoints yColumn"
		exit(0)
	        
	spec = Spectrum( energyRange, nPoints, profile, width )
	spec.loadAbsorptions( fileName, column )
	spec.buildProfile()
	
	print spec.spectrumData
	
if __name__ == "__main__":
        main()