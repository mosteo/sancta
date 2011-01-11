unit_length "m"
unit_angle  "radians"

# Common parameters to all robots
define base position (
  color "red"
  drive "diff"
  gui_nose 1                  # Indicate the front.
  obstacle_return 0           # Cannot hit things.
  laser_return 1              # Robot body seen by other lasers.

  localization "gps"          # Change to "gps" to have impossibly perfect, global odometry
  localization_origin [0 0 0] # Start odometry at (0, 0, 0).
  odom_error [ 0.01 0.01 0.020 ]   # Odometry error or slip in X, Y and Theta
                                   # (Uniform random distribution), proportional
                                   # to velocity.

  # Maximum limits:
  max_speed [1.0 0 1.74]   # m(x), m(y), radians(theta)
)

define quicklaser laser (
  samples 45
  range_min 0
  range_max 32
  fov 3.1415926535
  size [0 0]
)

define triangle base (

  # Body shape:
  size [1.5 1]
  polygons 1
  polygon[0].points 4
  polygon[0].point[0] [1 0.1]
  polygon[0].point[1] [1 -0.1]
  polygon[0].point[2] [-0.5 -0.5]
  polygon[0].point[3] [-0.5  0.5]

  #height 0.27724

  #sicklms200 ( pose [0.125 0 0] )
)


