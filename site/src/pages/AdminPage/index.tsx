import { FC, useCallback, useEffect, useState } from 'react';
import Reports from '../../component/Report/Reports';
import Verify from '../../component/Verify/Verify';
import Error from '../../component/Error/Error';
import { useLocation } from 'react-router-dom';
import trpc from '../../trpc';

const AdminPage: FC = () => {
  const location = useLocation();
  const [loaded, setLoaded] = useState<boolean>(false);
  const [authorized, setAuthorized] = useState<boolean>(false);

  // user has to be authenticated as admin to view this page
  const checkAdmin = useCallback(async () => {
    const res = await trpc.users.get.query();
    setAuthorized(res.isAdmin);
    setLoaded(true);
  }, []);

  useEffect(() => {
    checkAdmin();
  }, [checkAdmin]);

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
