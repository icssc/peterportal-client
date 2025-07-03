import { FC, useCallback, useEffect, useState } from 'react';
import Reports from './Reports/Reports';
import Verify from './Verify/Verify';
import Error from '../../component/Error/Error';
import { useLocation } from 'react-router-dom';
import trpc from '../../trpc';

const AdminPage: FC = () => {
  const location = useLocation();
  const [loaded, setLoaded] = useState<boolean>(false);
  const [authorized, setAuthorized] = useState<boolean>(false);

  // user has to be authenticated as admin to view this page
  const checkAdmin = useCallback(async () => {
    trpc.users.get
      .query()
      .then((res) => {
        setAuthorized(res.isAdmin);
        setLoaded(true);
      })
      .catch(() => {
        // If a user is not logged in, they can't be authorized
        setAuthorized(false);
        setLoaded(true);
      });
  }, []);

  useEffect(() => {
    checkAdmin();
  }, [checkAdmin]);

  /** @todo replace the loading text here with LoadingSpinner once that gets merged */
  if (!loaded) {
    return <p>Loading...</p>;
  } else if (!authorized) {
    return <Error message="Access Denied: You are not authorized to view this page."></Error>;
  } else {
    if (location.pathname.includes('reports')) {
      return <Reports />;
    } else if (location.pathname.includes('verify')) {
      return <Verify />;
    }
  }
  return <Error message="Invalid Admin Page"></Error>;
};

export default AdminPage;
