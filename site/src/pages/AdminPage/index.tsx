import axios, { AxiosResponse } from 'axios';
import { FC, useEffect, useState } from 'react';
import Reports from '../../component/Report/Reports';
import Verify from '../../component/Verify/Verify';
import Error from '../../component/Error/Error';
import './AdminPage.scss';
import { useLocation } from 'react-router-dom';

const AdminPage: FC = () => {
  const location = useLocation();
  const [loaded, setLoaded] = useState<boolean>(false);
  const [authorized, setAuthorized] = useState<boolean>(false);

  interface AdminResponse {
    admin: boolean;
  }

  // user has to be authenticated as admin to view this page
  const checkAdmin = async () => {
    const res: AxiosResponse<AdminResponse> = await axios.get('/api/users/isAdmin');
    const isAdmin: boolean = res.data.admin;
    setAuthorized(isAdmin);
    setLoaded(true);
  };

  useEffect(() => {
    checkAdmin();
  }, []);

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
