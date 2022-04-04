import AppBar from '@material-ui/core/AppBar';
import Box from '@material-ui/core/Box';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import CardHeader from '@material-ui/core/CardHeader';
import Divider from '@material-ui/core/Divider';
import Tab from '@material-ui/core/Tab';
import Tabs from '@material-ui/core/Tabs';
import Typography from '@material-ui/core/Typography';
import { useRouter } from 'next/router';
import { useState } from 'react';
import styled from 'styled-components';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import { UserPasswordForm, UserProfileForm } from './forms';

export const Account: React.FC = () => {
  const router = useRouter();
  const { isAuthenticated, isCheckingAuth, user, setUser } = useAuthentication();

  const [activeTabIndex, setActiveTabIndex] = useState(0);

  const handleChange = (event: React.ChangeEvent<unknown>, newValue: number) => {
    setActiveTabIndex(newValue);
  };
  if (isCheckingAuth) {
    return <></>; // TODO: Improve loading, create Loader component, test with Slow 3G
  }

  if (!isAuthenticated) {
    router.push('/login');
    return <></>;
  }

  return (
    <ResponsiveContainer maxWidth="xl">
      <Typography component="h1" variant="h5">
        Settings
      </Typography>
      <ContentWrapper>
        <AppBar position="static" color="inherit">
          <Tabs value={activeTabIndex} onChange={handleChange} aria-label="simple tabs example">
            <Tab label="General" {...a11yProps(0)} />
            <Tab label="Password" {...a11yProps(1)} />
            <Tab label="Patreon" {...a11yProps(2)} />
          </Tabs>
        </AppBar>
        <TabPanel activeTabIndex={activeTabIndex} index={0}>
          <UserProfileForm user={user} setUser={setUser} />
          <div style={{ textAlign: 'center', marginTop: '5px' }}>
            <em>Changes here will only take place on the new MTG CB.</em>
          </div>
        </TabPanel>
        <TabPanel activeTabIndex={activeTabIndex} index={1}>
          <UserPasswordForm user={user} setUser={setUser} />
          <div style={{ textAlign: 'center', marginTop: '5px' }}>
            <em>Changes here will only take place on the new MTG CB.</em>
          </div>
        </TabPanel>
        <TabPanel activeTabIndex={activeTabIndex} index={2}>
          <Card>
            <CardHeader title="Patreon" titleTypographyProps={{ variant: 'h6' }} />
            <Divider />
            <CardContent>
              This section is in progress. :) Immortal/Reserved List tier patrons will get the ability to customize a card to represent
              their collection in a hall of fame.
            </CardContent>
            <CardActions />
          </Card>
        </TabPanel>
      </ContentWrapper>
    </ResponsiveContainer>
  );
};

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(5),
}));

const TabPanelWrapper = styled(Box)(({ theme }) => ({
  marginTop: theme.spacing(3),
}));

interface TabPanelProps {
  index: number;
  activeTabIndex: number;
}

const TabPanel: React.FC<TabPanelProps> = (props) => {
  const { children, activeTabIndex, index, ...other } = props;

  return (
    <div role="tabpanel" hidden={activeTabIndex !== index} id={`tabpanel-${index}`} aria-labelledby={`tab-${index}`} {...other}>
      {activeTabIndex === index && <TabPanelWrapper>{children}</TabPanelWrapper>}
    </div>
  );
};

function a11yProps(index: number) {
  return {
    id: `tab-${index}`,
    'aria-controls': `tabpanel-${index}`,
  };
}
