type
  {$IFDEF FPC}
  TObjectList<T: TObject> = class(TFPGObjectList<T>);
  TList<T> = class(TFPGList<T>);
  {$ENDIF}

  {$IFDEF FMX}
  TAnyRect = TRectF;
  TAnyPoint = TPointF;
  TAnySize = TSizeF;
  TAnyColor = TAlphaColor;
  TAnyBitmap = FMX.{$IFDEF USE_NEW_UNITS}Graphics{$ELSE}Types{$ENDIF}.TBitmap;
  TAnyPicture = FMX.{$IFDEF USE_NEW_UNITS}Graphics{$ELSE}Types{$ENDIF}.TBitmap;
  {$ELSE}
  TAnyRect = TRect;
  TAnyPoint = TPoint;
  TAnySize = TSize;
  TAnyColor = TColor;
  TAnyBitmap = {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap;
  TAnyPicture = TPicture;
  {$ENDIF}

  TPixels = {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};

{$IFDEF FMX}
const clNone = TAlphaColors.Null;
{$ENDIF}

