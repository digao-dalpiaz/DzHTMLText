unit UFrmExample;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, HTLabel;

type
  TForm1 = class(TForm)
    Lb: THTLabel;
    procedure LbLinkClick(Sender: TObject; LinkID: Integer; Target: string;
      var Handled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Vcl.Dialogs;

procedure TForm1.LbLinkClick(Sender: TObject; LinkID: Integer; Target: string;
  var Handled: Boolean);
begin
  if Target='MSG_BOX' then
  begin
    ShowMessage('You have clicked at message box link!');
    Handled := True;
  end;
end;

end.
