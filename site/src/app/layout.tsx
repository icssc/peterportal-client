import type { Metadata } from 'next';

import 'bootstrap/dist/css/bootstrap.min.css';
import 'toastify-js/src/toastify.css';
import '../globals.scss';
import '../App.scss';

import AppHeader from '../component/AppHeader/AppHeader';
import ChangelogModal from '../component/ChangelogModal/ChangelogModal';
import SideBar from '../component/SideBar/SideBar';

// Import Global Store
import AppProvider from '../component/AppProvider/AppProvider';
import { createServerSideTrpcCaller } from '../trpc';
import { headers } from 'next/headers';

import { Roboto, Open_Sans } from 'next/font/google';

const roboto = Roboto({
  subsets: ['latin'],
  variable: '--font-roboto',
  style: ['normal', 'italic'],
  display: 'swap',
});

const openSans = Open_Sans({
  subsets: ['latin'],
  variable: '--font-open-sans',
  weight: ['400', '500', '600', '700'],
  style: ['normal', 'italic'],
  display: 'swap',
});

export const metadata: Metadata = {
  description:
    'A web application for course discovery and planning at UCI, featuring an enhanced catalogue and a 4-year planner.',
};

export default async function RootLayout({ children }: { children: React.ReactNode }) {
  const reqHeaders = await headers().then((h) => Object.fromEntries(h.entries()));
  const serverTrpc = createServerSideTrpcCaller(reqHeaders);
  const user = await serverTrpc.users.get.query().catch(() => null);

  return (
    <html
      lang="en"
      data-theme={user?.theme}
      className={`${roboto.variable} ${openSans.variable}`}
      suppressHydrationWarning
    >
      <head>
        <meta name="color-scheme" content="dark light" />
        <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png" />
        <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png" />
        <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png" />
        <link rel="manifest" href="/site.webmanifest" />
        <link rel="mask-icon" href="/safari-pinned-tab.svg" color="#5bbad5" />
        <meta name="theme-color" content="#121212" media="(prefers-color-scheme: dark)" />
        <meta name="theme-color" content="#ffffff" media="(prefers-color-scheme: light)" />
        <title>PeterPortal</title>
        {/* eslint-disable-next-line @next/next/no-sync-scripts */}
        <script src="/theme-script.js"></script>
        {/* This script must run and apply styles to the root HTML element before the
        <body> tag opens to avoid an unstyled body tag causing a white flash in dark mode */}
      </head>
      <body>
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
