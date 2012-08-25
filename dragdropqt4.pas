{ native DnD using the Qt4 API

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

{ Drag/Drop implementation using the Qt4 API.
  In this implementation the drag source will
  ask for the data immediately at the beginning
  of the drag/drop operation. StartDrag() will
  block until operation is finished.
}
unit DragDropQt4;

{$mode objfpc}{$H+}

interface
uses
  NativeDnD;

const
  DRAG_SOURCE_IMPLEMENTED = True;

procedure StartDrag(Src: TNativeDragSource);


implementation
uses
  Classes,
  qtwidgets,
  qt4;

{$note will this leak memory?}

procedure StartDrag(Src: TNativeDragSource);
var
  U8Str: UTF8String;
  WStr: WideString;
  FileList: TStringList;
  StrFileList: UTF8String;
  MimeName: WideString;
  ByteArray: QByteArrayH = nil;
  Widget: QWidgetH;
  Mime: QMimeDataH = nil;
  Drag: QDragH;

  function MimeObj: QMimeDataH;
  begin
    if not Assigned(Mime) then
      Mime := QMimeData_create();
    Result := Mime;
  end;

begin
  Widget := TQtWidget(src.Control.Handle).Widget;

  if Assigned(Src.OnDragGetStringData) then begin
    Src.CallOnDragStringData(U8Str);
    if Length(U8Str) > 0 then begin
      WStr := UTF8Decode(U8Str);
      QMimeData_setText(MimeObj, @WStr);
    end;
  end;

  if Assigned(Src.OnDragGetFileList) then begin
    FileList := Src.CallOnDragGetFileList;
    if FileList.Count > 0 then begin
      // we don't have QMimeData_setUrls() in libQt4Pas
      // unfortunately, so we must fiddle around with the
      // raw data of a "text/uri-list" entry manually.
      StrFileList := '';
      for U8Str in FileList do begin
        StrFileList += 'file://' + U8Str + chr(13) + chr(10);
      end;
      ByteArray := QByteArray_create(@StrFileList[1], Length(StrFileList));
      MimeName := 'text/uri-list';
      QMimeData_setData(MimeObj, @MimeName, ByteArray);
    end;
  end;

  if Assigned(Mime) then begin
    Drag := QDrag_create(Widget);
    QDrag_setMimeData(Drag, Mime);
    QDrag_exec(Drag, QtCopyAction);
  end;

  Src.CallOnDragEnd;

  if Assigned(ByteArray) then QByteArray_destroy(ByteArray);
end;


end.

