
convert_datatype <- function(d) {
  switch(
    d,
    Byte = "INT1U",
    UInt16 = "INT2U",
    Int16 = "INT2S",
    UInt32 = "INT4U",
    Int32 = "INT4S",
    Float32 = "FLT4S",
    Float64 = "FLT8S",
    INT1U = "Byte",
    INT2U = "UInt16",
    INT2S = "Int16",
    INT4U = "UInt32",
    INT4S = "Int32",
    FLT4S = "Float32",
    FLT8S = "Float64"
  )
}
