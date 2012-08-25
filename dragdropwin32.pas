{ native DnD using Win32 OLE

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

{ On Windows the drag source is not connected to any
  particular control or window as far as windows is
  concerned. When our LCL code detects the initiating
  mouse move it calls StartDrag() which will enter a
  modal event loop during which it will make various
  calls to our event methods and not return until the
  drag/drop operation is finished.
}
unit DragDropWin32;

{$mode objfpc}{$H+}

interface
uses
  NativeDnD;

procedure StartDrag(Src: TNativeDragSource);


implementation
uses
  sysutils,
  Classes,
  Windows,
  ActiveX,
  shlobj;

const
  TGT_FILELIST: FORMATETC = (
    CfFormat : CF_HDROP;
    Ptd      : nil;
    dwAspect : DVASPECT_CONTENT;
    lindex   : -1;
    tymed    : TYMED_HGLOBAL;
  );

  TGT_UNICODETEXT: FORMATETC = (
    CfFormat : CF_UNICODETEXT;
    Ptd      : nil;
    dwAspect : DVASPECT_CONTENT;
    lindex   : -1;
    tymed    : TYMED_HGLOBAL;
  );

  TGT_TEXT: FORMATETC = (
    CfFormat : CF_TEXT;
    Ptd      : nil;
    dwAspect : DVASPECT_CONTENT;
    lindex   : -1;
    tymed    : TYMED_HGLOBAL;
  );

type

  { TDropSource }

  TDropSource = class(TInterfacedObject, IDropSource)
  private
    FSrc: TNativeDragSource;
  public
    constructor Create(Src: TNativeDragSource);
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult; StdCall;
    function GiveFeedback(dwEffect: DWORD): HResult; StdCall;
  end;

  { TDataObject }

  TDataObject = class(TInterfacedObject, IDataObject)
  public
    constructor Create(Src: TNativeDragSource);
    function GetData(const formatetcIn: FORMATETC; out medium: STGMEDIUM): HRESULT; STDCALL;
    function GetDataHere(const pformatetc: FormatETC; out medium: STGMEDIUM): HRESULT; STDCALL;
    function QueryGetData(const pformatetc: FORMATETC): HRESULT; STDCALL;
    function GetCanonicalFormatEtc(const pformatetcIn: FORMATETC; out pformatetcOut: FORMATETC): HResult; STDCALl;
    function SetData(const pformatetc: FORMATETC; const medium: STGMEDIUM; FRelease: BOOL): HRESULT; StdCall;
    function EnumFormatEtc(dwDirection: DWord; out enumformatetcpara: IENUMFORMATETC): HRESULT; StdCall;
    function DAdvise(const formatetc: FORMATETC; advf: DWORD; const AdvSink: IAdviseSink; out dwConnection: DWORD): HRESULT; StdCall;
    function DUnadvise(dwconnection: DWord): HRESULT; StdCall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; StdCall;
  private
    FSrc: TNativeDragSource;
    FFmtList: array of FORMATETC;
    function HaveThisFormat(const AFmt: TFORMATETC): Boolean;
    procedure AddFormat(Fmt: FORMATETC);
  end;

{$note SHCreateStdEnumFmtEtc() definition in shlobj is wrong, report this bug}
function SHCreateStdEnumFmtEtc(cfmt:UINT; afmt: PFORMATETC; var ppenumFormatEtc:IEnumFORMATETC):HRESULT;StdCall;external 'shell32' name 'SHCreateStdEnumFmtEtc';

procedure StartDrag(Src: TNativeDragSource);
var
  DataObject: IDataObject;
  DropSource: IDropSource;
  DWEffect: DWord;
begin
  DataObject := TDataObject.Create(Src);
  DropSource := TDropSource.Create(Src);
  DoDragDrop(DataObject, DropSource, DROPEFFECT_COPY, @DWEffect);
  Src.CallOnDragEnd;
end;

{ TDataObject }

constructor TDataObject.Create(Src: TNativeDragSource);
begin
  inherited Create;
  FSrc := Src;
  if Assigned(Src.OnDragGetFileList) then begin
    AddFormat(TGT_FILELIST);
  end;
  if Assigned(Src.OnDragGetStringData) then begin
    AddFormat(TGT_UNICODETEXT);
    AddFormat(TGT_TEXT);
  end;
end;

function TDataObject.GetData(const formatetcIn: FORMATETC; out medium: STGMEDIUM): HRESULT; STDCALL;
var
  FileList: TStringList;       // Filenames must be UTF8 encoded
  FileName: UTF8String;
  BufLen: PtrInt;
  hgDropFiles: THANDLE;
  pgDropFiles: PDROPFILES;
  StringData: UTF8String;
  WideStringData: WideString;
  AnsiStringData: AnsiString;
  hgStringData: THandle;
  pgStringData: PWideChar;

  procedure SetResult(Handle: THandle);
  begin
    if Handle = 0 then
      Result := DV_E_FORMATETC
    else begin
      medium.Tymed := TYMED_HGLOBAL;
      medium.HGLOBAL := Handle;
      medium.PUnkForRelease := nil;
      Result := S_OK;
    end;
  end;

begin
  // This method may be called multiple times and its called already
  // during the drag operation, even when no drop has happened yet.

  if HaveThisFormat(formatetcIn) then begin
    case formatetcIn.CfFormat of
      CF_HDROP:
      begin
        FileList := TStringList.Create;
        FSrc.CallOnDragGetFileList(FileList);

        // First we need a widestring #0 sepatated and #0#0 at the end.
        WideStringData := '';
        for FileName in FileList do begin
          WideStringData += UTF8Decode(FileName) + WideChar(0);
        end;
        WideStringData += WideChar(0);
        FileList.Free;

        // now we need to allocate memory for a DROPFILES structure.
        // we need room for that structure plus the above widestring
        BufLen := SizeOf(DROPFILES) + Length(WideStringData) * SizeOf(WideChar);
        hgDropFiles := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT, BufLen);

        // populate the DROPFILES structure, it has the string of
        // filenames appended directly at the end
        pgDropFiles := GlobalLock(hgDropFiles);
        pgDropFiles^.pFiles := SizeOf(DROPFILES); // offset of the file list
        pgDropFiles^.fWide := True; // contains widestring
        Move(WideStringData[1], pgDropFiles[1], Length(WideStringData) * SizeOf(WideChar));
        GlobalUnlock(hgDropFiles);

        SetResult(hgDropFiles);
      end;

      CF_UNICODETEXT:
      begin
        FSrc.CallOnDragStringData(StringData);
        if Length(StringData) > 0 then begin
          WideStringData := UTF8Decode(StringData) + WideChar(0);
          BufLen := Length(WideStringData) * SizeOf(WideChar);

          hgStringData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT, BufLen);
          pgStringData := GlobalLock(hgStringData);
          Move(WideStringData[1], pgStringData[0], BufLen);
          GlobalUnlock(hgStringData);

          SetResult(hgStringData);
        end
        else
          SetResult(0);
      end;

      CF_TEXT:
      begin
        FSrc.CallOnDragStringData(StringData);
        if Length(StringData) > 0 then begin
          AnsiStringData := Utf8ToAnsi(StringData) + Char(0);
          BufLen := Length(AnsiStringData);

          hgStringData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT, BufLen);
          pgStringData := GlobalLock(hgStringData);
          Move(AnsiStringData[1], pgStringData[0], BufLen);
          GlobalUnlock(hgStringData);

          SetResult(hgStringData);
        end
        else
          SetResult(0);
      end;
    end;
  end
  else
    SetResult(0);
end;

function TDataObject.GetDataHere(const pformatetc: FormatETC; out medium: STGMEDIUM): HRESULT; STDCALL;
begin
end;

function TDataObject.QueryGetData(const pformatetc: FORMATETC): HRESULT; STDCALL;
begin
  if HaveThisFormat(pformatetc) then
    Result := S_OK
  else
    Result := DV_E_FORMATETC;
end;

function TDataObject.GetCanonicalFormatEtc(const pformatetcIn: FORMATETC; out pformatetcOut: FORMATETC): HResult; STDCALl;
begin
end;

function TDataObject.SetData(const pformatetc: FORMATETC; const medium: STGMEDIUM; FRelease: BOOL): HRESULT; StdCall;
begin
end;

function TDataObject.EnumFormatEtc(dwDirection: DWord; out enumformatetcpara: IENUMFORMATETC): HRESULT; StdCall;
begin
  Result := E_NOTIMPL;
  enumformatetcpara := nil;
  if dwDirection = DATADIR_GET then
    if Length(FFmtList) > 0 then
      Result := SHCreateStdEnumFmtEtc(Length(FFmtList), @FFmtList[0], enumformatetcpara)
end;

function TDataObject.DAdvise(const formatetc: FORMATETC; advf: DWORD; const AdvSink: IAdviseSink; out dwConnection: DWORD): HRESULT; StdCall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.DUnadvise(dwconnection: DWord): HRESULT; StdCall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult; StdCall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.HaveThisFormat(const AFmt: TFORMATETC): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FFmtList) - 1 do begin
    with FFmtList[I] do begin
      if ((AFmt.tymed and tymed) <> 0)
      and (AFmt.CfFormat = CfFormat) then begin
        exit(True);
      end;
    end;
  end;
end;

procedure TDataObject.AddFormat(Fmt: FORMATETC);
var
  I: Integer;
begin
  I := Length(FFmtList);
  SetLength(FFmtList, I + 1);
  FFmtList[I] := Fmt;
end;

{ TDragSource }

constructor TDropSource.Create(Src: TNativeDragSource);
begin
  inherited Create;
  FSrc := Src;
end;

function TDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult; StdCall;
begin
  // if the Escape key has been pressed since the last call, cancel the drop
  if fEscapePressed = True then
    exit(DRAGDROP_S_CANCEL);

  // if the LeftMouse button has been released, then do the drop!
  if (grfKeyState and MK_LBUTTON) = 0 then
    exit(DRAGDROP_S_DROP);

  // continue with the drag-drop
  Result := S_OK;
end;

function TDropSource.GiveFeedback(dwEffect: DWORD): HResult; StdCall;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize();
end.

