{$IFDEF FMX}
  {$IF CompilerVersion >= 26} //XE5
    {$DEFINE USE_NEW_UNITS}
  {$ENDIF}
  {$IF CompilerVersion >= 29} //XE8
    {$DEFINE USE_NEW_ENV}
    {$DEFINE USE_IMGLST}
  {$ENDIF}
{$ELSE}
  {$DEFINE VCL}
  {$DEFINE USE_NEW_ENV}
  {$DEFINE USE_IMGLST}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 3175 off : Some fields coming before "$1" were not initialized}
{$WARN 3177 off : Some fields coming after "$1" were not initialized}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5028 off : Local $1 "$2" is not used}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$ENDIF}

{$ZEROBASEDSTRINGS OFF}
