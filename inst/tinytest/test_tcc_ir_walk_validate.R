library(Rtinycc)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# --- IR constructors cover all node shapes ---

base_node <- Rtinycc:::tcc_ir_node("x", mode = "double")
expect_equal(base_node$tag, "x")
expect_equal(base_node$mode, "double")

const_node <- Rtinycc:::tcc_ir_const(1.5, "double")
expect_equal(const_node$tag, "const")
expect_equal(const_node$value, 1.5)
expect_equal(const_node$mode, "double")

var_node <- Rtinycc:::tcc_ir_var("x", "integer")
expect_equal(var_node$name, "x")
expect_equal(var_node$mode, "integer")

param_node <- Rtinycc:::tcc_ir_param("p", "logical")
expect_equal(param_node$name, "p")
expect_equal(param_node$mode, "logical")

unary_node <- Rtinycc:::tcc_ir_unary("-", var_node, "double")
expect_equal(unary_node$tag, "unary")
expect_equal(unary_node$op, "-")
expect_equal(unary_node$x$name, "x")

binary_node <- Rtinycc:::tcc_ir_binary("+", var_node, const_node, "double")
expect_equal(binary_node$tag, "binary")
expect_equal(binary_node$op, "+")
expect_equal(binary_node$lhs$name, "x")
expect_equal(binary_node$rhs$value, 1.5)

if_node <- Rtinycc:::tcc_ir_if(var_node, const_node, const_node, "double")
expect_equal(if_node$tag, "if")
expect_equal(if_node$yes$value, 1.5)
expect_equal(if_node$no$value, 1.5)

call_node <- Rtinycc:::tcc_ir_call("sqrt", list(var_node), "double")
expect_equal(call_node$tag, "call")
expect_equal(call_node$fun, "sqrt")
expect_equal(call_node$args[[1]]$name, "x")

assign_node <- Rtinycc:::tcc_ir_assign("acc", const_node, "double", kind = "scalar")
expect_equal(assign_node$tag, "assign")
expect_equal(assign_node$name, "acc")
expect_equal(assign_node$kind, "scalar")

block_node <- Rtinycc:::tcc_ir_block(list(assign_node), const_node)
expect_equal(block_node$tag, "block")
expect_equal(block_node$mode, "double")
expect_equal(block_node$result$value, 1.5)

for_node <- Rtinycc:::tcc_ir_for("i", call_node, block_node)
expect_equal(for_node$tag, "for")
expect_equal(for_node$mode, "void")
expect_equal(for_node$var, "i")

alloc_node <- Rtinycc:::tcc_ir_vec_alloc(const_node)
expect_equal(alloc_node$tag, "vec_alloc")
expect_equal(alloc_node$mode, "sexp")

get_node <- Rtinycc:::tcc_ir_vec_get(var_node, const_node, mode = "double")
expect_equal(get_node$tag, "vec_get")
expect_equal(get_node$arr$name, "x")

set_node <- Rtinycc:::tcc_ir_vec_set(var_node, const_node, unary_node)
expect_equal(set_node$tag, "vec_set")
expect_equal(set_node$mode, "void")

length_node <- Rtinycc:::tcc_ir_length(var_node)
expect_equal(length_node$tag, "length")
expect_equal(length_node$mode, "integer")

rf_node <- Rtinycc:::tcc_ir_rf_call("mean", list(var_node), mode = "sexp")
expect_equal(rf_node$tag, "rf_call")
expect_equal(rf_node$fun, "mean")
expect_equal(rf_node$mode, "sexp")

fb_node <- Rtinycc:::tcc_ir_fallback("unsupported")
expect_equal(fb_node$tag, "fallback")
expect_equal(fb_node$mode, "sexp")
expect_equal(fb_node$reason, "unsupported")

# --- Walkers and boundary scanners ---

seen_calls <- 0L
seen_leaves <- 0L
w_counts <- Rtinycc:::tccq_make_walker(
  call = function(e, w) {
    seen_calls <<- seen_calls + 1L
    for (ee in as.list(e)[-1]) {
      if (!missing(ee)) Rtinycc:::tccq_walk(ee, w)
    }
  },
  leaf = function(e, w) {
    seen_leaves <<- seen_leaves + 1L
    invisible(NULL)
  }
)
Rtinycc:::tccq_walk(quote(f(a, 1L, g(b))), w_counts)
expect_true(seen_calls >= 2L)
expect_true(seen_leaves >= 3L)

handler_hit <- FALSE
w_handler <- Rtinycc:::tccq_make_walker(
  handler = function(v, w) {
    if (identical(v, "sin")) {
      function(e, w) {
        handler_hit <<- TRUE
        invisible(NULL)
      }
    }
  }
)
Rtinycc:::tccq_walk(quote(sin(1)), w_handler)
expect_true(handler_hit)

expect_true(Rtinycc:::tccq_has_boundary(quote(.Call("x", y))))
expect_true(Rtinycc:::tccq_has_boundary(quote(f(.Primitive("sqrt")(x)))))
expect_false(Rtinycc:::tccq_has_boundary(quote(a + b * c)))

arrays <- Rtinycc:::tccq_collect_subset_arrays(quote({
  x[i]
  y[j]
  x[k]
  z[i, j]
}))
expect_equal(sort(arrays), c("x", "y"))

scan <- Rtinycc:::tccq_scan_constructs(list(
  quote(for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      out <- out + i + j
    }
  }),
  quote(if (a > 0) a else 0)
))
expect_true(scan$has_for)
expect_equal(scan$max_loop_depth, 2L)
expect_true("for" %in% scan$calls)
expect_true("if" %in% scan$calls)

scan_empty <- Rtinycc:::tccq_scan_constructs(list())
expect_false(scan_empty$has_for)
expect_equal(scan_empty$max_loop_depth, 0L)
expect_equal(length(scan_empty$calls), 0L)

# --- IR traversal / validation catches hidden malformed nodes ---

hidden_tag_ir <- list(
  tag = "root",
  mode = "void",
  args = list(
    list(tag = "child", mode = "double"),
    list(tag = "target", mode = "double")
  )
)
expect_true(Rtinycc:::tccq_ir_has_tag(hidden_tag_ir, "target"))
expect_false(Rtinycc:::tccq_ir_has_tag(hidden_tag_ir, "absent"))

expect_true(Rtinycc:::tccq_validate_ir(list(
  tag = "mean_expr",
  mode = "double",
  expr = list(tag = "const", mode = "double", value = 1)
), fallback = "soft"))

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "rf_call", mode = "double"),
    fallback = "soft"
  ),
  pattern = "rf_call missing mode/shape/contract"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "rf_call", mode = "sexp", shape = "scalar", contract = list()),
    fallback = "soft"
  ),
  pattern = "unsupported mode"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "rf_call", mode = "double", shape = "tensor", contract = list()),
    fallback = "soft"
  ),
  pattern = "unsupported shape"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "rf_call", mode = "double", shape = "scalar", contract = list()),
    fallback = "hard"
  ),
  pattern = "forbids Rf_eval"
)

expect_error(
  Rtinycc:::tccq_validate_ir(list(tag = "mean_expr", mode = "double"), fallback = "soft"),
  pattern = "missing expr"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "quantile_expr", mode = "double", expr = list(tag = "const")),
    fallback = "soft"
  ),
  pattern = "missing expr/prob"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "quantile_vec_expr", mode = "double", expr = list(tag = "const")),
    fallback = "soft"
  ),
  pattern = "missing expr/probs"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "matmul", mode = "double", a = list(tag = "const")),
    fallback = "soft"
  ),
  pattern = "matmul missing operands"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "solve_lin", mode = "double", a = list(tag = "const"), b = list(tag = "const")),
    fallback = "soft"
  ),
  pattern = "solve_lin missing operands"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "transpose", mode = "double"),
    fallback = "soft"
  ),
  pattern = "transpose missing operand"
)

expect_error(
  Rtinycc:::tccq_validate_ir(
    list(tag = "mat_reduce", mode = "double"),
    fallback = "soft"
  ),
  pattern = "mat_reduce missing fields"
)

hidden_bad_ir <- list(
  tag = "block",
  mode = "void",
  stmts = list(
    list(tag = "matmul", mode = "double", a = list(tag = "const"))
  ),
  result = list(tag = "const", mode = "double", value = 0)
)
expect_error(
  Rtinycc:::tccq_validate_ir(hidden_bad_ir, fallback = "soft"),
  pattern = "matmul missing operands"
)

# --- Wrapper path selection ---

id_scalar <- function(x) {
  declare(type(x = double(1)))
  x
}
decl <- Rtinycc:::tcc_quick_parse_declare(id_scalar)
ir <- Rtinycc:::tcc_quick_lower(id_scalar, decl)
built <- Rtinycc:::tcc_quick_compile(id_scalar, decl, ir)
expect_true(identical(typeof(built$call_ptr), "externalptr"))

wrapped_ptr <- Rtinycc:::tcc_quick_make_wrapper(
  built$callable,
  formals(id_scalar),
  built$compiled,
  id_scalar,
  compiled_ptr = built$call_ptr
)
expect_equal(wrapped_ptr(7), id_scalar(7), tolerance = 1e-12)
expect_true(grepl(
  ".rtinycc_call",
  paste(deparse(body(wrapped_ptr)), collapse = " "),
  fixed = TRUE
))

called_fallback_path <- FALSE
callable_stub <- function(x, env) {
  called_fallback_path <<- TRUE
  x + 1
}
wrapped_callable <- Rtinycc:::tcc_quick_make_wrapper(
  callable_stub,
  formals(function(x) NULL),
  compiled_obj = list(),
  fn = function(x) x,
  compiled_ptr = NULL
)
expect_equal(wrapped_callable(2), 3)
expect_true(called_fallback_path)
