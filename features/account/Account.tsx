import { useState } from 'react';
import { useRouter } from 'next/router';
import styled from 'styled-components';
import AppBar from '@material-ui/core/AppBar';
import Box from '@material-ui/core/Box';
import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import CardHeader from '@material-ui/core/CardHeader';
import Container from '@material-ui/core/Container';
import Divider from '@material-ui/core/Divider';
import Tab from '@material-ui/core/Tab';
import Tabs from '@material-ui/core/Tabs';
import Typography from '@material-ui/core/Typography';
import Link from '../../components/Link';
import { useAuthentication } from '../../auth/AuthenticationProvider';
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

  // TODO: Make a nice layout of breadcrumbs, container, and so on, like a normal page wrapper/layout
  // TODO: Figure out proper max width and nesting
  return (
    <Container maxWidth="lg">
      <Breadcrumbs separator=">" aria-label="breadcrumb">
        <Link href="/" variant="body2" color="inherit">
          MTG CB
        </Link>
        <Link href="#" variant="body2" color="inherit">
          Account
        </Link>
      </Breadcrumbs>
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
        </TabPanel>
        <TabPanel activeTabIndex={activeTabIndex} index={1}>
          <UserPasswordForm user={user} setUser={setUser} />
        </TabPanel>
        <TabPanel activeTabIndex={activeTabIndex} index={2}>
          <Card>
            <CardHeader title="Patreon" titleTypographyProps={{ variant: 'h6' }} />
            <Divider />
            <CardContent>
              I need to someday include Patreon information, like my patreon email address and supporter card, or integrate with their new
              widget.
            </CardContent>
            <CardActions>Placeholder</CardActions>
          </Card>
        </TabPanel>
      </ContentWrapper>
    </Container>
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
