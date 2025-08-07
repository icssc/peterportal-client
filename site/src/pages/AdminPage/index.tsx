import { FC, useEffect, useState } from 'react';
import Reports from './Reports/Reports';
import Verify from './Verify/Verify';
import Error from '../../component/Error/Error';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import { useLocation } from 'react-router-dom';
import trpc from '../../trpc';

const AdminPage: FC = () => {
  const location = useLocation();
  const [loaded, setLoaded] = useState<boolean>(false);
  const [authorized, setAuthorized] = useState<boolean>(false);

  useEffect(() => {
    trpc.users.get
      .query()
      .then((res) => setAuthorized(res.isAdmin))
      .catch(() => setAuthorized(false))
      .finally(() => setLoaded(true));
  }, []);

  if (!loaded) {
    return <LoadingSpinner />;
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
