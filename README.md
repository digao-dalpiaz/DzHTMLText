# HTLabel
## Delphi HTML Label component

This visual component allows you to specify a formatted text in a label, using almost the same sintax used in HTML code.

Here are all possible tags you can use in text:

```
<A[:abc]></A> - Link
<B></B> - Bold
<I></I> - Italic
<U></U> - Underline
<S></S> - Strike out
<FN:abc></FN> - Font Name
<FS:123></FS> - Font Size
<FC:clColor|$999999></FC> - Font Color
<BC:clColor|$999999></BC> - Background Color
<BR> - Line Break
<L></L> - Align Left
<C></C> - Align Center
<R></R> - Aligh Right
<T:123></T> - Tab
<TF:123></TF> - Tab with aligned break
```

## Installing

Just add the HTLabel.pas to a package. Then build and install.

Note: To ensure the component is displayed with its icon, add the following line to the Package Source:
```
{$R HTLabel.dcr}
```

Supports Delphi XE2..Delphi 10.3 Rio

## Component properties

`AutoHeight` = Auto set height of control when Text property changed

`AutoWidth` = Auto set width of control when Text property changed.
If you are using AutoWidth, the text never wraps to a new line unless a line break is specifyed at text.

`AutoOpenLink` = Open links automatically on click over, without set event OnLinkClick.
This property calls ShellExecute method.

`Color` = Backgroud color of control

`Font` = Determines the base font. When no tag is specifyed on text, this base font is used.

`Lines` = Returns the total lines of text, according to the bounds of control. This property is read-only.

`MaxWidth` = Specify the maximum width of text, even when the control bounds is bigger. This is useful when using AutoWidth property.

`StyleLinkNormal` = Properties to formatting a link when is not selected by mouse.

`StyleLinkHover` = Properties to formatting a link when is selected by mouse.

`Text` = The text you want to show at label control. You can use <BR> tag to break lines.

`TextHeight` = Returns the total text height. This property is read-only.

`TextWidth` = Returns the total text width. This property is read-only.

