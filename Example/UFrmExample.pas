unit UFrmExample;

interface

uses Vcl.Forms, Vcl.ImgList, Vcl.Controls, System.Classes, System.ImageList,
  Vcl.DzHTMLText, Vcl.Graphics;

type
  TForm1 = class(TForm)
    Lb: TDzHTMLText;
    MyImages: TImageList;
    Lb2: TDzHTMLText;
    Lb3: TDzHTMLText;
    procedure FormCreate(Sender: TObject);
    procedure LbRetrieveImgRes(Sender: TObject; const ResourceName: string;
      Picture: TPicture; var Handled: Boolean);
    procedure LbLinkClick(Sender: TObject; Link: TDHBaseLink;
      var Handled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Vcl.Dialogs;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.LbLinkClick(Sender: TObject; Link: TDHBaseLink;
  var Handled: Boolean);
begin
  if Link.LinkRef.Target='INFO_ABOUT' then
  begin
    ShowMessage('This is the example app.');
    Handled := True;
  end;
end;

procedure TForm1.LbRetrieveImgRes(Sender: TObject; const ResourceName: string;
  Picture: TPicture; var Handled: Boolean);
begin
  if ResourceName='LOGO' then
  begin
    //Load image by myself
    Picture.Assign(Application.Icon);

    Handled := True; //tell the component to NOT load resource automatically
  end;
end;

end.
