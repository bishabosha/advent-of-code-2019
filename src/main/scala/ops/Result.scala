package ops

enum Result with
  case Success
  case Error(msg: String)
