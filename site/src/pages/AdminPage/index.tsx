import { FC, useEffect, useState, useContext } from 'react';
import { Button } from 'react-bootstrap';
import './AdminPage.scss';

import Error from '../../component/Error/Error';
import Reports from '../../component/Report/Reports';
import Verify from '../../component/Verify/Verify';
import trpc from '../../trpc';
import ThemeContext from '../../style/theme-context';

type AdminTab = 'Verify Reviews' | 'View Reports';

interface ListSelectorProps {
  text: AdminTab;
  selectedTab: AdminTab;
  setSelectedTab: (tab: AdminTab) => void;
}
const ListSelector: FC<ListSelectorProps> = ({ text, selectedTab, setSelectedTab }) => {
  const { darkMode } = useContext(ThemeContext);
  const variant = selectedTab === text ? 'primary' : darkMode ? 'dark' : 'light';
  const selectTab = () => setSelectedTab(text);

  return (
    <Button variant={variant} className="admin-tab-button" onClick={selectTab}>
      <span>{text}</span>
    </Button>
  );
};

const AdminPage: FC = () => {
  const [loading, setLoading] = useState(true);
  const [authorized, setAuthorized] = useState(false);
  const [selectedTab, setSelectedTab] = useState<AdminTab>('Verify Reviews');

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
      <div className="admin-tabs-container">
        <div className="admin-tabs">
          <ListSelector text="Verify Reviews" selectedTab={selectedTab} setSelectedTab={setSelectedTab} />
          <ListSelector text="View Reports" selectedTab={selectedTab} setSelectedTab={setSelectedTab} />
        </div>
      </div>
      {selectedTab === 'Verify Reviews' ? <Verify /> : <Reports />}
    </>
  );
};

export default AdminPage;
