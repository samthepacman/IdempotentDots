while true; do
  read -p "Exit Sway - (yes/no) : " yn
    case $yn in
        [Yy]* ) notify-send "Exiting sway ( in 3 sec )" && sleep 3 && pkill sway; break;;
        [Nn]* ) notify-send "Cancelled"; break;;
        * ) echo "Please answer yes or no.";;
    esac
done

