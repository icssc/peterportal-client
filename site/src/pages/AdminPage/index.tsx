import { FC } from 'react';
import Reports from '../../component/Report/Reports';
import Verify from '../../component/Verify/Verify';
import Error from '../../component/Error/Error';
import { useLocation } from 'react-router-dom';
import { useAppSelector } from '../../store/hooks';
import { selectIsAdmin } from '../../store/slices/userSlice';

const AdminPage: FC = () => {
  const location = useLocation();
  const isAdmin = useAppSelector(selectIsAdmin);

  if (!isAdmin) {
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
