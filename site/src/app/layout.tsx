import type { Metadata } from 'next';

import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import 'toastify-js/src/toastify.css';
import '../style/theme.scss';
import '../globals.scss';
import '../App.scss';
import '../index.css';

import AppHeader from '../component/AppHeader/AppHeader';
import ChangelogModal from '../component/ChangelogModal/ChangelogModal';
import SideBar from '../component/SideBar/SideBar';

// Import Global Store
import AppProvider from '../component/AppProvider/AppProvider';
import { createServerSideTrpcCaller } from '../trpc';
import { headers } from 'next/headers';

export const metadata: Metadata = {
  description:
    'A web application for course discovery and planning at UCI, featuring an enhanced catalogue and a 4-year planner.',
};

export default async function RootLayout({ children }: { children: React.ReactNode }) {
  const reqHeaders = await headers().then((h) => Object.fromEntries(h.entries()));
  const serverTrpc = createServerSideTrpcCaller(reqHeaders);
  const user = await serverTrpc.users.get.query().catch(() => null);

  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png" />
        <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png" />
        <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png" />
        <link rel="manifest" href="/site.webmanifest" />
        <link rel="mask-icon" href="/safari-pinned-tab.svg" color="#5bbad5" />
        <meta name="theme-color" content="#ffffff" />
        <title>PeterPortal</title>
      </head>
      <body data-theme={user?.theme} suppressHydrationWarning>
        <script src="/theme-script.js"></script>
        <AppProvider user={user}>
          <div id="root">
            <AppHeader />
            <div className="app-body">
              <SideBar />
              <div className="app-content">{children}</div>
              <ChangelogModal />
            </div>
          </div>
        </AppProvider>
      </body>
    </html>
  );
}
