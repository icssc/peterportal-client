'use client';
/** @todo refactor this file */
import { FC, useEffect, useState } from 'react';
import Reports from './reports/Reports';
import Verify from './verify/Verify';
import Error from '../../component/Error/Error';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import trpc from '../../trpc';
import { AdminTab } from '@peterportal/types';

const AdminPage: FC<{ activeTab: AdminTab }> = ({ activeTab }) => {
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
    if (activeTab === 'reports') {
      return <Reports />;
    } else if (activeTab === 'verify') {
      return <Verify />;
    }
  }
  return <Error message="Invalid Admin Page"></Error>;
};

export default AdminPage;
