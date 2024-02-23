{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDzHTMLText;

{$warn 5023 off : no warning about unused units}
interface

uses
  Vcl.DHCommon, Vcl.DHTokenEngine, Vcl.DzHTMLTextReg, Vcl.DzHTMLText, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Vcl.DzHTMLTextReg', @Vcl.DzHTMLTextReg.Register);
end;

initialization
  RegisterPackage('LazDzHTMLText', @Register);
end.
