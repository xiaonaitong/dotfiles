alias vimdiff='vim -d'

#mount virtualbox share dir
function mount-share ()
{
    [ ! -d ~/host ] && mkdir ~/host
    sudo mount -t vboxsf share ~/host
}

function install-oracle-jdk ()
{
    local install_script=$1
    local script=$(basename $install_script)
    chmod a+x "$install_script"
    if [ -d "/usr/lib/jvm" ]; then
        echo "/usr/lib/jvm exist, you may have insalled oracle jdk"
        exit 1
    fi
    
    sudo mkdir /usr/lib/jvm
    sudo mv "$install_script" /usr/lib/jvm/
    cd /usr/lib/jvm
    sudo "./$script"
    local installed_dir="$(pwd)/$(ls |grep -v "$script")"
    sudo ln -s -b ${installed_dir}/bin/java /etc/alternatives/java
    sudo ln -s -b ${installed_dir}/bin/java /usr/bin/java
    sudo ln -s -b ${installed_dir}/bin/javac /usr/bin/javac
}

function add-path ()
{
    for p in $@
    do
        export PATH=$p:$PATH
    done
}

#show hardware info
function show-hardinfo()
{
    sudo lshw
}

#sbt download
#use http://xsbt.org/sbt-launch-0.12.1.jar instead of
#http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.12.1/sbt-launch.jar

function install-emacs()
{
    sudo add-apt-repository ppa:cassou/emacs
}

#verify end data of ssl certificate
#check-ssl-certificate www14.software.ibm.com
function check-ssl-certificate() {

    if [ $# -lt 1 ] ; then
        echo "usage: check-ssl-certificate host [port]"
        exit 1
    fi
    
    local defaultport=443
    local host=$1
    local port=${2:-$defaultport}

    openssl s_client -connect $host:$port 2>/dev/null | \
            openssl x509 -text|grep "Not After"
}

## make system image and copy to remote host
function install-sshfs() {
    sudo apt-get install sshfs
    sudo modprobe fuse
    
    sudo adduser fuse
    sudo chown root:fuse /dev/fuse
    sudo chmod +x /dev/fusermount

    sshfs vm:/home/xiaonaitong/host vmhost
    sudo dd if=/dev/sda1 | gzip > ./vmhost/ubuntu-dev.img.gz
}

# monitor net speed
# first install speedometer: sudo apt-get install speedometer
function net-speed() {
    speedometer -t eth0 -r eth0
}


# port forward
function port-forward() {
	socat TCP-LISTEN:8080,fork TCP:192.168.27.100:8080
}

## for simple java project
## use ant and maven repo to resolve dependencies
# <project default="sync-lib" xmlns:artifact="urn:maven-artifact-ant" >
#   <target name="sync-lib" depends="initmvn">
#     <delete dir="lib" />
#     <mkdir dir="lib" />
#     <artifact:dependencies filesetId="jclouds.fileset" versionsId="dependency.versions">
#       <dependency groupId="org.jclouds" artifactId="jclouds-all" version="1.5.7" />
#       <dependency groupId="org.jclouds.driver" artifactId="jclouds-sshj" version="1.5.7" />
#       <dependency groupId="ch.qos.logback" artifactId="logback-classic" version="[1.0.0,)" />
#     </artifact:dependencies>
#     <copy todir="lib" verbose="true">
#       <fileset refid="jclouds.fileset"/>
#       <mapper type="flatten" />
#     </copy>
#   </target>
  
#   <get src="http://search.maven.org/remotecontent?filepath=org/apache/maven/maven-ant-tasks/2.1.3/maven-ant-tasks-2.1.3.jar" dest="maven-ant-tasks.jar"/>
  
#   <target name="initmvn">
#     <path id="maven-ant-tasks.classpath" path="maven-ant-tasks.jar"/>
#     <typedef resource="org/apache/maven/artifact/ant/antlib.xml"
#              uri="urn:maven-artifact-ant"
#              classpathref="maven-ant-tasks.classpath"/>
#   </target>
# </project>

function create-svn-project-repo() {
    local projectname=$1
    local inits=/opt/svn-project-skeleton
    local user="sudo -u www-data "
    $user svnadmin create /opt/svn-repo/$projectname
    $user svn import $inits file:///opt/svn-repo/$projectname -m "init project"
}

function add-svn-user() {
    # create svn auth file
    # sudo htpasswd -cs /opt/share/svn/svn-auth-file username
    sudo htpasswd -s /opt/share/svn/svn-auth-file username
}

function create-git-project-repo() {
    local projectname=$1
    local user="sudo -u www-data "
    local projectrepo="/var/www/git/$projectname.git"
    
    $user mkdir $projectrepo

    (
        cd $projectrepo;
        $user git --bare init;
        $user git update-server-info;
    )
}
# .npmrc
# registry = http://registry.npmjs.vitecho.com

function install-bals() {
    # adobeair may can't find gnome-keyring
    sudo ln -s /usr/lib/i386-linux-gnu/libgnome-keyring.so.0.2.0 /usr/lib/libgnome-keyring.so.0.2.0
    sudo ln -s /usr/lib/i386-linux-gnu/libgnome-keyring.so.0 /usr/lib/libgnome-keyring.so.0
    sudo dpkg -i Downloads/adobeair_2.6.0.19170_i386.deb
    sudo dpkg -i Downloads/MockupsForDesktop32bit.deb
}

# yum install from cd/dvd
function yum-install-from-dvd() {
    mount /dev/dvd /mnt
    cat > /etc/yum.repos.d/local.repo <<EOF
[local]
Name="Local Installation"
baseurl="file:///mnt"
enabled=1
gpgcheck=0
EOF

    yum groupinstall "X Window System"
}

# mysql reset root password
# detail at http://dev.mysql.com/doc/refman/5.0/en/resetting-permissions.html
function mysql-reset-root-pass() {
    local newpass=$1
    sudo mysqld_safe --skip-grant-tables
    mysql <<EOF
    UPDATE mysql.user SET Password=PASSWORD('$newpass')  WHERE User='root';
    FLUSH PRIVILEGES;
EOF

    # grant remote root access
    mysql <<EOF
    create user 'root'@'%' identified by $newpass;
    grant all on *.* to 'root'@'%';
    FLUSH PRIVILEGES;
EOF
}

# html css tips {
#    overflow: hidden // see http://colinaarts.com/articles/the-magic-of-overflow-hidden/
# }

# zip without .git and node_modules
function zip-without-git-and-node() {
    local dest=$1
    zip -9 -r --exclude=*.git*  --exclude=*node_modules* foo.zip $dest
}

# batch remove aws security groups
function aws-remove-sg(){
    aws ec2 describe-security-groups \
        |grep GroupName |awk -F " " '{print $2}'|tr -d '",'|sort|uniq \
        | xargs -I {} -n 1 aws ec2  delete-security-group --group-name "{}"
}

# uwsgi
function uwsgi-start(){
    # cd /home/xiao/try/distaster-tools/dragonfly/src
    uwsgi --socket 127.0.0.1:8081 --wsgi-file dragonfly.py
}

# ruby rvm usage
function rvm-setup(){
    rvm use 1.9.3@rails --create
    echo "rvm use 1.9.3@rails" > .rvmrc 
}

# about python attribute and methods
# see http://www.cafepy.com/article/python_attributes_and_methods/python_attributes_and_methods.html#attribute-search-summary
