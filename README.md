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
