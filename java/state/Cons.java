package state;

import java.util.Objects;

public final class Cons {

    private Object car;
    private Object cdr;

    public Cons(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public Object getCar() {
        return car;
    }

    public void setCar(Object car) {
        this.car = car;
    }

    public Object getCdr() {
        return cdr;
    }

    public void setCdr(Object cdr) {
        this.cdr = cdr;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Cons cons = (Cons) o;
        return Objects.equals(car, cons.car) &&
                Objects.equals(cdr, cons.cdr);
    }

    @Override
    public int hashCode() {
        return Objects.hash(car, cdr);
    }

    @Override
    public String toString() {
        return String.format("Cons{car=%s, cdr=%s}", car, cdr);
    }
}
