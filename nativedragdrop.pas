{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NativeDragDrop;

interface

uses
  NativeDnD, nativedragdrop_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('nativedragdrop_reg', @nativedragdrop_reg.Register);
end;

initialization
  RegisterPackage('NativeDragDrop', @Register);
end.
