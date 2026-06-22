program test_RealHistogram
    use RealHistogram_
    use TestUtils_
    use GOptions_
    use Math_
    use String_
    use IOStream_
    use RealList_
    use Grid_
    use RNFunction_
    implicit none
		type(RealHistogram) :: histogram
		type(RealHistogram) :: histogramRunning
		
		histogram = RealHistogram( Histogram_SQUAREROOT )
		
		call histogram%add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call histogram%add( [23.21883_8, 25.35600_8, 28.41117_8, 22.08219_8, 19.55053_8, 23.63690_8, 27.07390_8, 25.11683_8] )
		call histogram%add( [24.07832_8, 22.04728_8, 29.07267_8, 23.84218_8, 24.07261_8, 23.97873_8, 25.67417_8, 23.89337_8] )
		call histogram%add( [23.49143_8, 26.14219_8, 22.87863_8, 21.59113_8, 23.56555_8, 26.42314_8, 23.51600_8, 26.27489_8] )
		call histogram%add( [21.07893_8, 20.48072_8, 24.90150_8, 23.17327_8, 23.81940_8, 25.11435_8, 26.52324_8, 18.73398_8] )
		call histogram%add( [24.09926_8, 23.07400_8, 26.71212_8, 21.77789_8, 25.51567_8, 25.13831_8, 22.11752_8, 22.47796_8] )
		call histogram%add( [25.39945_8, 26.71204_8, 25.67166_8, 22.52061_8, 23.62552_8, 26.00762_8, 25.37902_8, 26.28057_8] )
		call histogram%add( [22.61389_8, 24.06349_8, 24.33601_8, 21.97826_8, 26.48619_8, 25.47802_8, 26.89355_8, 26.07590_8] )
		call histogram%add( [21.74619_8, 21.99553_8, 23.40948_8, 25.48071_8, 23.02762_8, 22.70441_8, 25.03438_8, 25.67790_8] )
		call histogram%add( [24.68533_8, 21.26442_8, 24.89509_8, 24.71221_8, 25.12706_8, 26.05145_8, 20.59260_8, 22.63209_8] )
		call histogram%add( [23.35024_8, 26.70019_8, 21.51930_8, 24.98537_8, 24.94632_8, 19.42552_8, 27.00687_8, 21.65142_8] )
		call histogram%add( [25.00371_8, 23.40407_8, 21.82391_8, 24.25161_8, 24.28748_8, 24.17388_8, 21.20663_8, 26.66869_8] )
		call histogram%add( [22.89491_8, 24.81186_8, 25.14049_8, 22.61879_8] )
		
		histogramRunning = RealHistogram( rule=Histogram_STURGES, algorithm=Histogram_RUNNING )
		call histogramRunning%add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call histogramRunning%add( [23.21883_8, 25.35600_8, 28.41117_8, 22.08219_8, 19.55053_8, 23.63690_8, 27.07390_8, 25.11683_8] )
		call histogramRunning%add( [24.07832_8, 22.04728_8, 29.07267_8, 23.84218_8, 24.07261_8, 23.97873_8, 25.67417_8, 23.89337_8] )
		call histogramRunning%add( [23.49143_8, 26.14219_8, 22.87863_8, 21.59113_8, 23.56555_8, 26.42314_8, 23.51600_8, 26.27489_8] )
		call histogramRunning%add( [21.07893_8, 20.48072_8, 24.90150_8, 23.17327_8, 23.81940_8, 25.11435_8, 26.52324_8, 18.73398_8] )
		call histogramRunning%add( [24.09926_8, 23.07400_8, 26.71212_8, 21.77789_8, 25.51567_8, 25.13831_8, 22.11752_8, 22.47796_8] )
		call histogramRunning%add( [25.39945_8, 26.71204_8, 25.67166_8, 22.52061_8, 23.62552_8, 26.00762_8, 25.37902_8, 26.28057_8] )
		call histogramRunning%add( [22.61389_8, 24.06349_8, 24.33601_8, 21.97826_8, 26.48619_8, 25.47802_8, 26.89355_8, 26.07590_8] )
		call histogramRunning%add( [21.74619_8, 21.99553_8, 23.40948_8, 25.48071_8, 23.02762_8, 22.70441_8, 25.03438_8, 25.67790_8] )
		call histogramRunning%add( [24.68533_8, 21.26442_8, 24.89509_8, 24.71221_8, 25.12706_8, 26.05145_8, 20.59260_8, 22.63209_8] )
		call histogramRunning%add( [23.35024_8, 26.70019_8, 21.51930_8, 24.98537_8, 24.94632_8, 19.42552_8, 27.00687_8, 21.65142_8] )
		call histogramRunning%add( [25.00371_8, 23.40407_8, 21.82391_8, 24.25161_8, 24.28748_8, 24.17388_8, 21.20663_8, 26.66869_8] )
		call histogramRunning%add( [22.89491_8, 24.81186_8, 25.14049_8, 22.61879_8] )

		call assert_equal( histogram%size(), 100, "RealHistogram_test: size Storing" )
		call assert_true( abs(histogram%mean() - 24.06056_8) < 1e-5_8, "RealHistogram_test: mean Storing" )
		call assert_true( abs(histogram%stdev() - 2.03038_8) < 1e-5_8, "RealHistogram_test: stdev Storing" )
		call assert_true( abs(histogram%minimum() - 18.73398_8) < 1e-5_8, "RealHistogram_test: minimum Storing" )
		call assert_true( abs(histogram%maximum() - 29.07267_8) < 1e-5_8, "RealHistogram_test: maximum Storing" )
		call assert_true( abs(histogram%stderr() - 0.20304_8) < 1e-5_8, "RealHistogram_test: stderr Storing" )

		call assert_true( abs(histogramRunning%mean() - 24.06056_8) < 1e-5_8, "RealHistogram_test: mean Running" )
		call assert_true( abs(histogramRunning%stdev() - 2.03038_8) < 1e-5_8, "RealHistogram_test: stdev Running" )
		call assert_true( abs(histogramRunning%stderr() - 0.20304_8) < 1e-5_8, "RealHistogram_test: stderr Running" )

		call histogram%build( binsPrecision=3 )
		call assert_true( histogram%counts%nPoints() > 0, "RealHistogram_test: nPoints after build" )
end program test_RealHistogram
