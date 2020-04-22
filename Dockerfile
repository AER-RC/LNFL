# Build LNFL and LBLRTM in the latest CentOS environment
FROM centos:6

# install tools necessary for building models
RUN yum -y install make gcc-gfortran && \
  yum -y -q update && \
  yum clean all
RUN rm -rf /var/cache/yum/*

WORKDIR /LNFL

# necessary code for LNFL
ADD aer_rt_utils /LNFL/aer_rt_utils
ADD build /LNFL/build
ADD src /LNFL/src

# line file parameter database
# stage files for model run
ADD line_file/aer_v_3.7 /LNFL/TAPE1
ADD line_file/lncpl_lines /LNFL/TAPE2
ADD extra_brd_params/co2_co2_brd_param /LNFL/co2_co2_brd_param
ADD extra_brd_params/o2_h2o_brd_param /LNFL/o2_h2o_brd_param
ADD extra_brd_params/o2_uv_brd_param /LNFL/o2_uv_brd_param
ADD extra_brd_params/wv_co2_brd_param /LNFL/wv_co2_brd_param
ADD extra_brd_params/co2_h2o_brd_param /LNFL/co2_h2o_brd_param
ADD extra_brd_params/o2_o2_brd_param /LNFL/o2_o2_brd_param
ADD spd_dep/spd_dep_param /LNFL/spd_dep_param

# build the model and clean up after the build
RUN cd /LNFL/build; \
  make -f make_lnfl linuxGNUsgl; \
  rm -rf lnfl_v3.2_linux_gnu_sgl.obj *.mod; cd /LNFL; \
  ln -s lnfl_v3.2_linux_gnu_sgl lnfl

# volume mount to at least the TAPE3 is necessary, otherwise user
# will not see output; other possible mounts: TAPE6, TAPE7
VOLUME LNFL_Out

# need to run as user rather than root, otherwise volume-mounted
# material is a mess
#USER lnfl_user

# consider this in `docker run`:
# Setup with current user and group
#"--user $(id -u):$(id -g)",

# Fix SELinux permissions issues
#"--privileged=true",

# also this: https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#user#user

COPY lnfl_entrypoint.sh .

# run model and build binary line file (TAPE3)
ENTRYPOINT ["./lnfl_entrypoint.sh"]
