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
