'use client';

// Import Global Store
import { store } from '../../store/store';
import { Provider } from 'react-redux';
import { PostHogProvider } from 'posthog-js/react';
import AppThemeProvider from '../AppThemeProvider/AppThemeProvider';
import { FC, PropsWithChildren } from 'react';
import { useLoadSavedCourses } from '../../hooks/savedCourses';

const UserDataLoader: FC = () => {
  useLoadSavedCourses();
  return null;
};

const AppProvider: FC<PropsWithChildren> = ({ children }) => {
  let appContent = (
    <>
      <UserDataLoader />
      <AppThemeProvider>{children}</AppThemeProvider>
    </>
  );

  const posthogKey = process.env.NEXT_PUBLIC_POSTHOG_KEY;
  const posthogHost = process.env.NEXT_PUBLIC_POSTHOG_HOST;
  if (posthogHost && posthogKey) {
    const posthogOptions = { api_host: posthogHost, autocapture: true, enable_heatmaps: true };
    appContent = (
      <PostHogProvider apiKey={posthogKey} options={posthogOptions}>
        {appContent}
      </PostHogProvider>
    );
  } else {
    // Send warning about misconfiguration, though on development this can be safely ignored
    console.warn('PostHog Host or Key was not provided');
  }

  return <Provider store={store}>{appContent}</Provider>;
};

export default AppProvider;
