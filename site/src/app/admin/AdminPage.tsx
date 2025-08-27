'use client';
/** @todo refactor this file */
import { FC } from 'react';
import Reports from './reports/Reports';
import Verify from './verify/Verify';
import Error from '../../component/Error/Error';
import { AdminTab } from '@peterportal/types';
import { useAppSelector } from '../../store/hooks';

const AdminPage: FC<{ activeTab: AdminTab }> = ({ activeTab }) => {
  const isAdmin = useAppSelector((state) => state.user.isAdmin);
  if (!isAdmin) {
    return <Error message="Access Denied: You are not authorized to view this page."></Error>;
  }
  if (activeTab === 'reports') {
    return <Reports />;
  } else if (activeTab === 'verify') {
    return <Verify />;
  }
  return <Error message="Invalid Admin Page"></Error>;
};

export default AdminPage;
