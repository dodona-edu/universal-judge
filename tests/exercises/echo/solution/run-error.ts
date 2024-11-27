function throw_error() {
    const obj: any = null;
    console.log(obj.does_not_exist());
}

throw_error()
