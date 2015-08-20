boost::variant<boost::recursive_wrapper<Var >,boost::recursive_wrapper<BinOp >,boost::recursive_wrapper<ListOp > > Expr;
struct Var{
	explicit Var (std::string _unVar):
		unVar_(_unVar){
	}
	std::string unVar_;
};
struct BinOp{
	explicit BinOp (Expr _left,std::string _op,Expr _right):
		left_(_left),op_(_op),right_(_right){
	}
	Expr left_;
	std::string op_;
	Expr right_;
};
struct ListOp{
	explicit ListOp (std::string _op,std::vector<Expr > _args):
		op_(_op),args_(_args){
	}
	std::string op_;
	std::vector<Expr > args_;
};

