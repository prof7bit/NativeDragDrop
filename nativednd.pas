{ Platform native Drag and Drop between applications.

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

{ Create an instance of this component, assign
  the control you want to act as drag source,
  assign one or more of the OnDragGet* events.
  If the event method is called then fill the
  supplied variable with data (for example add
  file names to the FileList). Note that these
  event methods may get called even before the
  drag operation is completed.

  It will only work if you set the control that
  you want to act as source to DragMode=dmManual,
  the internal LCL Drag mechanism is incompatible.

  The FileNames and String contents that you
  supply are expected to be UTF8 encoded.

  This is not yet complete, features are still
  missing and also it works only with GTK2, Qt4
  and Win32 at the moment. I started this as a
  quick and pragmatic hack to make my application
  work. Ideally at some later time all the methods
  and properties of this component should be made
  a part of the LCL TWinControl class itself.
}
unit NativeDnD;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  Controls;


type

  TDragBeginEvent = procedure(Sender: TObject; MouseX, MouseY: Integer) of object;
  TDragFileListEvent = procedure(Sender: TObject; FileList: TStringList) of object;
  TDragStringDataEvent = procedure(Sender: TObject; out StringData: UTF8String) of object;

  { TNativeDragSource }

  TNativeDragSource = class(TComponent)
    constructor Create(AOwner: TComponent); override;
  protected
    FControl: TWinControl;
    FGetFileListEvent: TDragFileListEvent;
    FGetStringDataEvent: TDragStringDataEvent;
    FBeginEvent: TDragBeginEvent;
    FEndEvent: TNotifyEvent;
    FOldMouseDown: TMouseEvent;
    FOldMouseMove: TMouseMoveEvent;
    FMouseDownPos: TPoint;
    FIsDragging: Boolean;
    FIsInitialized: Boolean;
    procedure UnsetControl;
    procedure SetControl(AControl: TWinControl);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure InstallMouseRedirection;
    procedure UninstallMouseRedirection;
  public
    InternalData: TObject; // widgetset code may store internal stuff here
    procedure CallOnDragBegin;
    procedure CallOnDragEnd;
    procedure CallOnDragGetFileList(FileList: TStringList);
    procedure CallOnDragStringData(out StringData: UTF8String);
    property IsDragging: Boolean read FIsDragging;
  published
    property Control: TWinControl read FControl write SetControl;
    property OnDragGetFileList: TDragFileListEvent read FGetFileListEvent write FGetFileListEvent;
    property OnDragGetStringData: TDragStringDataEvent read FGetStringDataEvent write FGetStringDataEvent;
    property OnDragBegin: TDragBeginEvent read FBeginEvent write FBeginEvent;
    property OnDragEnd: TNotifyEvent read FEndEvent write FEndEvent;
  end;

procedure Register;

implementation
uses
  DragDropDummy,
  {$ifdef LCLGTK2}DragDropGtk2,{$endif}
  {$ifdef LCLWIN32}DragDropWin32,{$endif}
  {$ifdef LCLQT}DragDropQt4,{$endif}
  typinfo,
  LResources;

{ TNativeDragSource }

constructor TNativeDragSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControl := nil;
  FGetFileListEvent := nil;
  FGetStringDataEvent := nil;
  FBeginEvent := nil;
  FEndEvent := nil;
  FMouseDownPos := Point(-1, -1);
  FIsDragging := False;
  FIsInitialized := False;
end;

procedure TNativeDragSource.UnsetControl;
begin
  if FControl = nil then
    exit;
  UninstallMouseRedirection;
  FinalizeDragSource(Self);
  FIsInitialized := False;
  FControl := nil;
end;

procedure TNativeDragSource.SetControl(AControl: TWinControl);
begin
  if FControl = AControl then
    exit;
  if Assigned(FControl) then
    UnsetControl;
  FControl := AControl;
  InstallMouseRedirection;
end;

procedure TNativeDragSource.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FMouseDownPos := Point(X, Y)
  else
    FMouseDownPos := Point(-1, -1);
  if Assigned(FOldMouseDown) then
    FOldMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TNativeDragSource.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  DragDist: Double;
begin
  if IsDragging then
    exit;
  if (FMouseDownPos.Y <> -1) and (FMouseDownPos.Y <> -1) then begin
    if ssLeft in Shift then begin
      DragDist := sqrt(sqr(X - FMouseDownPos.X) + sqr(Y - FMouseDownPos.Y));
      if DragDist > 5 then begin
        if not FIsInitialized then begin
          InitializeDragSource(Self);
          FIsInitialized := True;
        end;
        CallOnDragBegin;
        StartDrag(Self);
        FMouseDownPos := Point(-1, -1);
      end;
    end
    else
      FMouseDownPos := Point(-1, -1);
  end;
  if Assigned(FOldMouseMove) then
    FOldMouseMove(Sender, Shift, X, Y);
end;

procedure TNativeDragSource.InstallMouseRedirection;
begin
  if DRAG_SOURCE_IMPLEMENTED then begin
    FOldMouseDown := TMouseEvent(GetMethodProp(Control, 'OnMouseDown'));
    FOldMouseMove := TMouseMoveEvent(GetMethodProp(Control, 'OnMouseMove'));
    SetMethodProp(Control, 'OnMouseMove', TMethod(@MouseMove));
    SetMethodProp(Control, 'OnMouseDown', TMethod(@MouseDown));
  end;
end;

procedure TNativeDragSource.UninstallMouseRedirection;
begin
  if DRAG_SOURCE_IMPLEMENTED then begin
    SetMethodProp(Control, 'OnMouseMove', TMethod(FOldMouseMove));
    SetMethodProp(Control, 'OnMouseDown', TMethod(FOldMouseDown));
  end;
end;

procedure TNativeDragSource.CallOnDragBegin;
begin
  FIsDragging := True;
  if Assigned(OnDragBegin) then
    OnDragBegin(Control, FMouseDownPos.X, FMouseDownPos.Y);
end;

procedure TNativeDragSource.CallOnDragGetFileList(FileList: TStringList);
begin
  if Assigned(OnDragGetFileList) then
    OnDragGetFileList(Control, FileList);
end;

procedure TNativeDragSource.CallOnDragStringData(out StringData: UTF8String);
begin
  if Assigned(OnDragGetStringData) then
    OnDragGetStringData(Control, StringData);
end;

procedure TNativeDragSource.CallOnDragEnd;
begin
  FIsDragging := False;
  if Assigned(OnDragEnd) then
    OnDragEnd(Control);
end;

procedure Register;
begin
  RegisterComponents('System', [TNativeDragSource]);
end;

initialization
  {$i nativednd.lrs}
end.

