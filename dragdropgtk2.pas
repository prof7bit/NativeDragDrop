{ native DnD using the GTLK2 API

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published
  by the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent
  modules,and to copy and distribute the resulting executable under terms
  of your choice, provided that you also meet, for each linked independent
  module, the terms and conditions of the license of that module. An
  independent module is a module which is not derived from or based on
  this library. If you modify this library, you may extend this exception
  to your version of the library, but you are not obligated to do so. If
  you do not wish to do so, delete this exception statement from your
  version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library
  General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{ Although GTK2 could start the drag automatically
  if we would use gtk_drag_source_set() we don't do
  it because some of the LCL controls are not based
  on native GTK Widgets and would not work. Because
  of this we only connect the signals and start the
  drag manually with gtk_drag_begin().
}
unit DragDropGtk2;

{$mode objfpc}{$H+}

interface
uses
  NativeDnD;

const
  DRAG_SOURCE_IMPLEMENTED = True;

procedure StartDrag(Src: TNativeDragSource); // need this in some cases


implementation
uses
  Classes,
  glib2,
  gtk2,
  gdk2;

const
  FMT_FILELIST = 1;
  FMT_TEXT = 2;
  TGT_FILE: TGtkTargetEntry = (target: 'text/uri-list'; flags: 0; info: FMT_FILELIST;);
  TGT_TEXT1: TGtkTargetEntry = (target: 'text/plain'; flags: 0; info: FMT_TEXT;);
  TGT_TEXT2: TGtkTargetEntry = (target: 'STRING'; flags: 0; info: FMT_TEXT;);

Type
  TDragSignalHandlers = class
    HDragEnd: gulong;
    HDragDataGet: gulong;
  end;

procedure GtkDragDataGet(GtkW: PGtkWidget;
                      Context: PGdkDragContext;
                      SelData: PGtkSelectionData;
                   TargetType: guint;
                         Time: guint;
                          Src: TNativeDragSource); cdecl;
var
  FileList: TStringList;
  StringData: UTF8String;
  I: Integer;
  A: array of PChar;
begin
  case TargetType of
    FMT_TEXT:
    begin
      Src.CallOnDragStringData(StringData);
      if Length(StringData) > 0 then begin
        gtk_selection_data_set_text(SelData, PChar(StringData), Length(StringData));
      end;;
    end;

    FMT_FILELIST:
    begin
      FileList := TStringList.Create;
      Src.CallOnDragGetFileList(FileList);
      if FileList.Count > 0 then begin
        SetLength(A, FileList.Count);
        for I := 0 to FileList.Count - 1 do begin
         FileList.Strings[I] := 'file://' + FileList.Strings[I];
         A[i] := PChar(FileList.Strings[I]);
        end;
        A[FileList.Count] := nil;
        gtk_selection_data_set_uris(SelData, @A[0]);
        FileList.Free;
      end;
    end;
  end;
end;

procedure DisconnectSignals(Src: TNativeDragSource); cdecl;
var
  GtkW: PGtkWidget;
  H: TDragSignalHandlers;
begin
  GtkW := PGtkWidget(Src.Control.Handle);
  H := TDragSignalHandlers(Src.InternalData);
  g_signal_handler_disconnect(GtkW, H.HDragDataGet);
  g_signal_handler_disconnect(GtkW, H.HDragEnd);
  H.Free;
  Src.InternalData := nil;
end;

procedure GtkDragEnd(GtkW: PGtkWidget;
                  Context: PGdkDragContext;
                      Src: TNativeDragSource); cdecl;
begin
  Src.CallOnDragEnd;
  DisconnectSignals(Src);
end;

procedure ConnectSignals(Src: TNativeDragSource); cdecl;
var
  GtkW: PGtkWidget;
  H: TDragSignalHandlers;
begin
  GtkW := PGtkWidget(Src.Control.Handle);
  H := TDragSignalHandlers.Create;
  Src.InternalData := H;
  H.HDragDataGet := g_signal_connect(GtkW, 'drag-data-get', TGCallback(@GtkDragDataGet), Src);
  H.HDragEnd := g_signal_connect(GtkW, 'drag-end', TGCallback(@GtkDragEnd), Src);
end;

procedure StartDrag(Src: TNativeDragSource);
var
  TargetCount: Integer;
  GtkW: PGtkWidget;
  TargetList: PGtkTargetList;

  procedure AddTarget(Target: TGtkTargetEntry);
  begin
    if TargetCount = 0 then
      TargetList := gtk_target_list_new(@Target, 1)
    else
      gtk_target_list_add_table(TargetList, @Target, 1);
    Inc(TargetCount);
  end;

begin
  TargetCount := 0;
  if Assigned(Src.OnDragGetFileList) then begin
    AddTarget(TGT_FILE);
  end;
  if Assigned(Src.OnDragGetStringData) then begin
    AddTarget(TGT_TEXT1);
    AddTarget(TGT_TEXT2);
  end;
  if TargetCount > 0 then begin
    ConnectSignals(Src);
    GtkW := PGtkWidget(Src.Control.Handle);
    gtk_drag_begin(GtkW, TargetList, GDK_ACTION_COPY, 1, nil);
  end;
end;

end.
