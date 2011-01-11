# Stage model definitions for ActivMedia robots
# Include into a Stage world file with 'include'
# Converted from ARIA parameter files by arparams2stage.
# $Id: PioneerRobotModels.world.inc,v 1.4 2005/06/08 17:25:08 reed Exp $

# SICK LMS-200 laser rangefinder
define sicklms200 laser (
  range_min 0.0
  range_max 32.0
  fov 180.0
  samples 361
  color "LightBlue"
  size [0.155 0.15]
  laser_return 1
  ranger_return 1
  blobfinder_return 0
  fiducial_return 0
  alwayson 1
)

# Sonar array
define pioneerSonar ranger (
  sview [0.1 4.0 30]
  ssize [0.01 0.04]
  laser_return 0
  blobfinder_return 0
  fiducial_return 0
)

# Model for a p3at differential-drive robot base with sonar.
define p3at position (
  color "red"
  drive "diff"                # Differential drive.
  gui_nose 1                  # Indicate the front.
  gui_boundary 0              # Don't draw bounding box.
  obstacle_return 1           # Can hit things.
  laser_return 1              # Robot body seen by other lasers.
  pioneerSonar_return 1             # Seen by other sonar.
  blobfinder_return 1         # Seen by other blobfinders.
  fiducial_return 2           # Seen as "2" by other fiducial sensors.

  # Speed profile:
  max_speed [0.75 0 100]
  accel [0.3 0 100]
  decel [0.3 0 100]

  # Body shape:
  size [0.626 0.505]
  polygons 1
  polygon[0].points 8
  polygon[0].point[0] [-0.18 0.313]
  polygon[0].point[1] [0.18 0.313]
  polygon[0].point[2] [0.2525 0.18]
  polygon[0].point[3] [0.2525 -0.18]
  polygon[0].point[4] [0.18 -0.313]
  polygon[0].point[5] [-0.18 -0.313]
  polygon[0].point[6] [-0.2525 -0.18]
  polygon[0].point[7] [-0.2525 0.18]
  polygon[0].filled 1

  # Sonar: 
  pioneerSonar (
    scount 16
    sview [0.1 4.0 30]
    ssize [0.01 0.04]
    spose[0] [0.147 0.136 90]
    spose[1] [0.193 0.119 50]
    spose[2] [0.227 0.079 30]
    spose[3] [0.245 0.027 10]
    spose[4] [0.245 -0.027 -10]
    spose[5] [0.227 -0.079 -30]
    spose[6] [0.193 -0.119 -50]
    spose[7] [0.147 -0.136 -90]
    spose[8] [-0.144 -0.136 -90]
    spose[9] [-0.189 -0.119 -130]
    spose[10] [-0.223 -0.079 -150]
    spose[11] [-0.241 -0.027 -170]
    spose[12] [-0.241 0.027 170]
    spose[13] [-0.223 0.079 150]
    spose[14] [-0.189 0.119 130]
    spose[15] [-0.144 0.136 90]
  )
  sicklms200( pose [0.125 0 0] )
)

# Model for a p3dx differential-drive robot base with sonar.
define p3dx position (
  color "red"
  drive "diff"                # Differential drive.
  gui_nose 1                  # Indicate the front.
  gui_boundary 0              # Don't draw bounding box.
  obstacle_return 1           # Can hit things.
  laser_return 1              # Robot body seen by other lasers.
  pioneerSonar_return 1             # Seen by other sonar.
  blobfinder_return 1         # Seen by other blobfinders.
  fiducial_return 2           # Seen as "2" by other fiducial sensors.

  # Speed profile:
  max_speed [0.75 0 100]
  accel [0.3 0 100]
  decel [0.3 0 100]

  # Body shape:
  origin [-0.04465 0.0 0.0]
  size [0.511 0.4]
  polygons 1
  polygon[0].points 8
  polygon[0].point[0] [-0.12 0.2555]
  polygon[0].point[1] [0.12 0.2555]
  polygon[0].point[2] [0.2 0.12]
  polygon[0].point[3] [0.2 -0.12]
  polygon[0].point[4] [0.12 -0.2555]
  polygon[0].point[5] [-0.12 -0.2555]
  polygon[0].point[6] [-0.2 -0.12]
  polygon[0].point[7] [-0.2 0.12]
  polygon[0].filled 1

  # Sonar: 
  pioneerSonar (
    scount 16
    sview [0.1 4.0 30]
    ssize [0.01 0.04]
    spose[0] [0.069 0.136 90]
    spose[1] [0.114 0.119 50]
    spose[2] [0.148 0.078 30]
    spose[3] [0.166 0.027 10]
    spose[4] [0.166 -0.027 -10]
    spose[5] [0.148 -0.078 -30]
    spose[6] [0.114 -0.119 -50]
    spose[7] [0.069 -0.136 -90]
    spose[8] [-0.157 -0.136 -90]
    spose[9] [-0.203 -0.119 -130]
    spose[10] [-0.237 -0.078 -150]
    spose[11] [-0.255 -0.027 -170]
    spose[12] [-0.255 0.027 170]
    spose[13] [-0.237 0.078 150]
    spose[14] [-0.203 0.119 130]
    spose[15] [-0.157 0.136 90]
  )
  sicklms200( pose [0 0 0] )
)

define omnibot position(
  color "dark green"
  drive "omni"

  gui_nose 1                  # Indicate the front.
  gui_boundary 0              # Don't draw bounding box.
  obstacle_return 1           # Can hit things.
  laser_return 1              # Robot body seen by other lasers.
  pioneerSonar_return 1       # Seen by other sonar.
  blobfinder_return 1         # Seen by other blobfinders.
  fiducial_return 2           # Seen as "2" by other fiducial sensors.

  max_speed [2.75 2.75 1.74]  # m, m, radians
  accel [0.3 0.1 2.0]         # m, m, radians
  decel [0.35 0.25 2.0]       # m, m, radians
  size [0.75 0.65]

  polygons 1
  polygon[0].points 8
  polygon[0].point[0] [-0.18 0.313]
  polygon[0].point[1] [0.18 0.313]
  polygon[0].point[2] [0.2525 0.18]
  polygon[0].point[3] [0.2525 -0.18]
  polygon[0].point[4] [0.18 -0.313]
  polygon[0].point[5] [-0.18 -0.313]
  polygon[0].point[6] [-0.2525 -0.18]
  polygon[0].point[7] [-0.2525 0.18]
  polygon[0].filled 1
  sicklms200( pose [0 0 0] )
)


# XXX HACK XXX lets you have more than one XXX XXX

define p3dx1 p3dx (
   color "red"
)

define p3dx2 p3dx (
   color "blue"
)

define p3dx3 p3dx (
   color "violet"
)

define p3dx4 p3dx (
   color "green"
)

define p3dx5 p3dx (
   color "orange"
)
