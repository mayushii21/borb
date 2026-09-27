// Left-hand mirror of the original right-hand USB holder.
// Original source: ../reference/things/boards/dartyl_choc/usb_holder.scad
module original_usb_holder() {
    include <../reference/things/boards/dartyl_choc/usb_holder.scad>
}

mirror([1, 0, 0]) original_usb_holder();
