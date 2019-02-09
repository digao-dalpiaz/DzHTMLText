unit UFrmExample;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, DzHTMLText;

type
  TForm1 = class(TForm)
    Lb: TDzHTMLText;
    procedure LbLinkClick(Sender: TObject; LinkID: Integer; Target: string;
      var Handled: Boolean);
    procedure LbMouseEnter(Sender: TObject);
    procedure LbMouseLeave(Sender: TObject);
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

procedure TForm1.LbMouseEnter(Sender: TObject);
begin
    Caption := 'OnMouseEnter';
end;

procedure TForm1.LbMouseLeave(Sender: TObject);
begin
    Caption := 'OnMouseLeave';
end;

end.
