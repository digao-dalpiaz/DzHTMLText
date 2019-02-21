unit UFrmExample;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, DzHTMLText;

type
  TForm1 = class(TForm)
    Lb: TDzHTMLText;
    procedure LbLinkClick(Sender: TObject; LinkID: Integer;
      LinkData: TDHLinkData; var Handled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Vcl.Dialogs;

procedure TForm1.LbLinkClick(Sender: TObject; LinkID: Integer;
  LinkData: TDHLinkData; var Handled: Boolean);
begin
  if LinkData.Target='MSG_BOX' then
  begin
    ShowMessage('You have clicked at message box link!');
    Handled := True;
  end;
end;

end.
