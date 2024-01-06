object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'FrmMain'
  ClientHeight = 662
  ClientWidth = 831
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object DzHTMLText1: TDzHTMLText
    Left = 8
    Top = 8
    Width = 153
    Height = 81
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    Color = clWhite
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
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    Color = clWhite
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Color = clWhite
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      'floating test:'
      '<nbr><div:x=200,y=20,width=60><c>my test panel 1234</c></div>'
      'Text after floating.'
      
        '<nbr><div:x=100,y=65,width=80><r><bc:clsilver><valign:center>And' +
        ' here a new floating panel abc'
      'Break <fs:20>BIG</fs></valign></bc></r></div>'
      '<bc:clyellow>Text <fs:18>after</fs> second panel</bc>')
    LineVertAlign = vaCenter
  end
  object DzHTMLText4: TDzHTMLText
    Left = 8
    Top = 280
    Width = 153
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    Color = clWhite
    ParentColor = False
    ParentFont = False
    Lines.Strings = (
      
        '<r><bc:clyellow>text<bc:clblue> </bc><bc:clred> </bc><bc:clgreen' +
        '> </bc>spacing test</bc></r>')
  end
  object DzHTMLText5: TDzHTMLText
    Left = 8
    Top = 191
    Width = 153
    Height = 81
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = []
    Color = clWhite
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Color = clWhite
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
  object DzHTMLText7: TDzHTMLText
    Left = 463
    Top = 8
    Width = 281
    Height = 195
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Lines.Strings = (
      'Line 1 (spacing in comp props)'
      'Line 2'
      'Line 3'
      ''
      'Line 5 (obs: autoheight enabled)')
    AutoHeight = True
    LineSpacing = 20
  end
  object DzHTMLText8: TDzHTMLText
    Left = 463
    Top = 209
    Width = 281
    Height = 180
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Lines.Strings = (
      '<bc:clYellow>Line 1'
      '<ls:5>Line 2 (Line Space: 5px)</ls>'
      'Line 3'
      ''
      'Line 5</bc> (obs: autoheight enabled)')
    AutoHeight = True
    LineSpacing = 20
  end
  object DzHTMLText9: TDzHTMLText
    Left = 169
    Top = 375
    Width = 289
    Height = 258
    Lines.Strings = (
      '<h:1>Header 1</h>'
      '<h:2>Header 2</h>'
      '<h:3>Header 3</h>'
      '<h:4>Header 4</h>'
      '<h:5>Header 5</h>'
      '<h:6>Header 6</h>'
      '<h:7>Header invalid</h>'
      '<style:custom>Custom Header</style>'
      
        '<i>text italic but <style:other>now using style without italic</' +
        'style></i>'
      
        '<style:custom>one style<style:other>inside another one</style></' +
        'style>')
    CustomStyles = <
      item
        Ident = 'Custom'
        FontColor = clDarkorange
      end
      item
        Ident = 'Other'
        StyleItalic = False
        BackColor = clGreen
      end>
    OverallVertAlign = vaCenter
    OverallHorzAlign = haCenter
  end
  object DzHTMLText10: TDzHTMLText
    Left = 464
    Top = 411
    Width = 281
    Height = 174
    Color = clCream
    ParentColor = False
    Lines.Strings = (
      
        '<fs:15><b>Some</b> <i>test</i> <u>with</u> <s>borders</s> and Au' +
        'toHeight</fs>'
      '<line:width=full>'
      ''
      '<line:width=200,height=10,color=clRed,coloralt=clBlue>'
      ''
      '<a:https://www.google.com/>Google</a>'
      ''
      '<fs:14>Formula = a<sup>2</sup> + t<sub>3</sub></fs>')
    AutoHeight = True
    Borders.All = 20
  end
  object DzHTMLText11: TDzHTMLText
    Left = 7
    Top = 375
    Width = 155
    Height = 83
    Lines.Strings = (
      'Testing offset (top and bottom)')
    AutoHeight = True
    Offset.Top = 20
    Offset.Bottom = 50
  end
  object DzHTMLText12: TDzHTMLText
    Left = 8
    Top = 464
    Width = 155
    Height = 184
    Lines.Strings = (
      '<bc:clCream>Testing offset (top and bottom)</bc>'
      
        '<offset:top=10><bc:clYellow>new line with specific offset</bc></' +
        'offset>'
      
        '<offset:top=5,bottom=10><bc:clCyan>new line with specific full o' +
        'ffset</bc></offset>')
    AutoHeight = True
    Offset.Top = 20
    Offset.Bottom = 50
  end
end
