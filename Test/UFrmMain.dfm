object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'FrmMain'
  ClientHeight = 568
  ClientWidth = 831
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DzHTMLText1: TDzHTMLText
    Left = 8
    Top = 8
    Width = 153
    Height = 81
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      'text spacing test')
  end
  object DzHTMLText2: TDzHTMLText
    Left = 8
    Top = 96
    Width = 153
    Height = 89
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      
        '<bc:clyellow>text<bc:clblue> </bc><bc:clred> </bc><bc:clgreen> <' +
        '/bc>spacing test</bc>')
  end
  object DzHTMLText3: TDzHTMLText
    Left = 168
    Top = 8
    Width = 289
    Height = 169
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      'floating test:'
      '<nbr><float:200,20,60><c>my test panel 1234</c></float>'
      'Text after floating.'
      
        '<nbr><float:100,65,80><r><bc:clsilver>And here a new floating pa' +
        'nel abc'
      'Break <fs:20>BIG</fs></bc></r></float>'
      '<bc:clyellow>Text <fs:18>after</fs> second panel</bc>')
    LineVertAlign = vaCenter
  end
  object DzHTMLText4: TDzHTMLText
    Left = 8
    Top = 280
    Width = 153
    Height = 89
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      
        '<r><bc:clyellow>text<bc:clblue> </bc><bc:clred> </bc><bc:clgreen' +
        '> </bc>spacing test</bc></r>')
  end
  object DzHTMLText5: TDzHTMLText
    Left = 8
    Top = 192
    Width = 153
    Height = 81
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      '<r>text spacing test</r>')
  end
  object DzHTMLText6: TDzHTMLText
    Left = 168
    Top = 184
    Width = 289
    Height = 185
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      '<b>Spoiler Test:</b>'
      '<spoiler:sp1>Click here for details</spoiler><sdetail:sp1>'
      '    Some detailed info about this spoiler.'
      '<spoiler:sp2>'
      '    Click here for sub-details</spoiler><sdetail:sp2>'
      
        '        Some detailed info about this sub-spoiler.</sdetail></sd' +
        'etail>'
      ''
      'You can open first spoiler <spoiler:sp1>here</spoiler> too.'
      ''
      
        '<sdetail:sp1><i>This is more info about the first spoiler</i></s' +
        'detail>')
  end
end
