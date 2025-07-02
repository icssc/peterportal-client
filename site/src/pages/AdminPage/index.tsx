import { FC, useEffect, useState } from 'react';
import './AdminPage.scss';

import Error from '../../component/Error/Error';
import Reports from '../../component/Report/Reports';
import Verify from '../../component/Verify/Verify';
import trpc from '../../trpc';

import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';

type AdminTab = 'verify' | 'reports';

const AdminPage: FC = () => {
  const [loading, setLoading] = useState(true);
  const [authorized, setAuthorized] = useState(false);
  const [currentTab, setCurrentTab] = useState<AdminTab>('verify');

  useEffect(() => {
    trpc.users.get
      .query()
      .then((res) => setAuthorized(res.isAdmin))
      .finally(() => setLoading(false));
  }, []);

  /** @todo replace the loading text here with LoadingSpinner once that gets merged */
  if (loading) {
    return <p>Loading...</p>;
  }

  if (!authorized) {
    return <Error message="Access Denied: You are not authorized to view this page." />;
  }

  return (
    <>
      <Tabs className="admin-tabs" value={currentTab} onChange={(_, newTab: AdminTab) => setCurrentTab(newTab)}>
        <Tab label="Verify Reviews" value="verify" />
        <Tab label="View Reports" value="reports" />
      </Tabs>
      {currentTab === 'verify' ? <Verify /> : <Reports />}
    </>
  );
};

export default AdminPage;
