NativeDragDrop
==============

This is a Lazarus package with a component that can act as a drag source
for drag-drop to other applications. Its using the widgetset API to
access the underlying platform DnD protocol.

The license is LGPL with static linking exception, so it can be used like
any other LCL component or maybe one day even included in Lazarus itself.

Currently implemented widgetsets:
  * Gtk2
  * Win32


Usage
-----
Install this package in Lazarus. Place the component on a form and
connect its Control property to one of the controls on your form.
Make sure that you leave the LCL internal drag/drop properties at
their default values (DragMode:=dmManual) so that LCL would not
attempt to invoke its own proprietary internal dragdrop procedure.

Implement one (or more) of the OnDragGetXxxx events. Which ones you
implement will deterimine which kind of data will be advertised to the
target application. The drop target will choose one of these formats
(if it supports it) and then call the corresponding OnDragGetXxx.

The drag sorce component will listen to OnMouseDown and OnMouseMove
events of the control to which it is connected and detect the drag
operation and start it when you click left and move 5 pixels. The
above mouse events will still work for your control and can be used
like normal, only when the drag gesture is detected the drag-drop
will take modal mouse control until the drag/drop operation ends.

Example
-------
Here is how to drag files. Of course this is a stupid example but it
demonstrates how the data is expected to look like.

    procedure TForm1.DSDragGetFileList(Sender: TObject; FileList: TStringList);
    begin
      // The TStringList exists already, just fill it with data
      // file names are expected to be UTF8 encoded.
      FileList.Append('/absolute/path/name/of/the/file');
      FileList.Append('/absolute/path/name/of/other/file');
    end;
