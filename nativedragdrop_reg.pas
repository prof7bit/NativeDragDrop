unit nativedragdrop_reg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation
uses
  NativeDnD,
  Classes,
  LResources;

procedure Register;
begin
  RegisterComponents('System', [TNativeDragSource]);
end;

initialization
  // the bitmaps for the component palette
  {$i tnativedragsource.lrs}
end.

