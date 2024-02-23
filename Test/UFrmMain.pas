unit UFrmMain;

interface

uses Vcl.Forms, System.Classes, Vcl.Controls, Vcl.DzHTMLText;

type
  TFrmMain = class(TForm)
    DzHTMLText1: TDzHTMLText;
    DzHTMLText2: TDzHTMLText;
    DzHTMLText3: TDzHTMLText;
    DzHTMLText4: TDzHTMLText;
    DzHTMLText5: TDzHTMLText;
    DzHTMLText6: TDzHTMLText;
    DzHTMLText7: TDzHTMLText;
    DzHTMLText8: TDzHTMLText;
    procedure FormCreate(Sender: TObject);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
