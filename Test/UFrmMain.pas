unit UFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DzHTMLText, Vcl.ExtCtrls;

type
  TFrmMain = class(TForm)
    DzHTMLText1: TDzHTMLText;
    DzHTMLText2: TDzHTMLText;
    DzHTMLText3: TDzHTMLText;
    DzHTMLText4: TDzHTMLText;
    DzHTMLText5: TDzHTMLText;
    DzHTMLText6: TDzHTMLText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

end.
