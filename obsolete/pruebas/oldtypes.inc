include "PioneerRobotModels.inc"

# A map
define map model
(
  color "black"

  gui_nose 0
  gui_boundary 1
  gui_grid 0
  gui_movemask 0
)

# big (false) p2at
define bigbot position (
  color "red"
  drive "diff"                # Differential drive.
  gui_nose 1                  # Indicate the front.
  gui_boundary 0              # Don't draw bounding box.
  obstacle_return 1           # Can hit things.
  laser_return 1              # Robot body seen by other lasers.
  pioneerSonar_return 1       # Seen by other sonar.
  blobfinder_return 1         # Seen by other blobfinders.
  fiducial_return 2           # Seen as "2" by other fiducial sensors.

  # Speed profile:
  max_speed [0.75 0 100]
  accel [0.3 0 100]
  decel [0.3 0 100]

  # Body shape:
  size [1.0 1.0]
  polygons 1
  polygon[0].points 8
  polygon[0].point[0] [-0.24 0.626]
  polygon[0].point[1] [0.24 0.616]
  polygon[0].point[2] [0.5050 0.24]
  polygon[0].point[3] [0.5050 -0.24]
  polygon[0].point[4] [0.24 -0.616]
  polygon[0].point[5] [-0.24 -0.616]
  polygon[0].point[6] [-0.5050 -0.24]
  polygon[0].point[7] [-0.5050 0.24]
  polygon[0].filled 1

  # LASER
  sicklms200( pose [0.125 0 0] )
)
