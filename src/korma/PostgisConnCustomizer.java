package korma;

import com.mchange.v2.c3p0.ConnectionCustomizer;

import java.sql.Connection;


/**
 * registers PGGeometry and PGBox3d with the postgres connection
 */
public class PostgisConnCustomizer implements ConnectionCustomizer {

    public void onAcquire(Connection connection, String s) throws Exception {
        ((org.postgresql.PGConnection)connection).addDataType("geometry",Class.forName("org.postgis.PGgeometry"));
        ((org.postgresql.PGConnection)connection).addDataType("box3d",Class.forName("org.postgis.PGbox3d"));
    }

    public void onDestroy(Connection connection, String s) throws Exception {}
    public void onCheckOut(Connection connection, String s) throws Exception {}
    public void onCheckIn(Connection connection, String s) throws Exception {}
}