import Typography from '@material-ui/core/Typography';
import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';

const ChangelogPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Typography variant="h3" component="h1" align="center">
      Changelog
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-11 (v0.34.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Allowed mobile sidenav to close when the user presses the region outside of it.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-09 (v0.33.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Made the site usable on mobile devices with a dozen related UI changes.</li>
        <li>Reverted the browse view to default to viewing sets first.</li>
        <li>Removed breadcrumbs from the top of pages -- they were just taking up space redundantly.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-03 (v0.32.2)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Fixed a bug causing blank search results due to filters changing after the user navigated to a further page of a previous search.
          The page will properly reset back to 1 now.
        </li>
        <li>Fixed a bug where the Collection menu item was not always active when viewing your collection.</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-02 (v0.32.1)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Fixed several client-side errors reported in Sentry.io -- viewing a table "too quickly" would cause a crash if data weren't
          available for it yet.
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      2022-01-01 (v0.32.0)
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Fixed a bug where unique cards completed weren't being calculated properly on the Set collection view.</li>
        <li>Made it clearer which pages are still not yet implemented.</li>
        <li>Corrected percentage rounding issue. (281/282 cards is 99%, not 100%.)</li>
        <li>Fixed the login issue on mobile devices / WebKit browsers.</li>
        <li>Changed the site and API to live on the mtgcollectionbuilder.com domain.</li>
        <li>Set up a redirect from the old domain to the new domain.</li>
      </ul>
    </Typography>
  </ResponsiveContainer>
);

export default ChangelogPage;
