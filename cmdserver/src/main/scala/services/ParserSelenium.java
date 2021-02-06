package services;
        import org.jsoup.Jsoup;
        import org.jsoup.nodes.Document;
        import org.jsoup.nodes.Element;
        import org.jsoup.select.Elements;
        import org.openqa.selenium.*;
        import org.openqa.selenium.chrome.ChromeDriver;
        import org.openqa.selenium.chrome.ChromeOptions;
        import org.openqa.selenium.firefox.FirefoxDriver;
        import org.openqa.selenium.remote.CapabilityType;
        import org.openqa.selenium.remote.DesiredCapabilities;
        import java.io.*;
        import java.util.*;
        import java.util.concurrent.TimeUnit;

/**
 * Class to compile a text file into a collection of valid twitter messages
 */
public class ParserSelenium {
    private WebDriver driver;
    private String baseUrl;
    private String adsUrl;
    @SuppressWarnings("MismatchedQueryAndUpdateOfStringBuilder")
    private StringBuffer verificationErrors = new StringBuffer();
    private static String currentDir = System.getProperty("user.dir");
    private static Boolean tweetsNSFW = false;

    private String username;
    private String password;
    private String companyCode = "";

    private long count = 0;


    private boolean isAnalyticsJsonSaved = false;

    private String baseFolder = "drivers";

    private String baseDownloadsFolder = "/tmp";
    private String downloadsFolder = baseDownloadsFolder;

    private long currentStartDate = 0;
    private long currentEndDate = 0;

    private boolean isFirsttime = false;

    private String twitterTweetsAnalyticPageSrc = "";

    private List<String> impressionsEngagementAndEngagementRatePer28Days = new ArrayList<>();

    public ParserSelenium() throws Exception {
        this.downloadsFolder = baseDownloadsFolder +  "/" + companyCode;
    }

    public void setBaseFolder(String folder) {

        this.baseFolder = folder;
    }

    public String getCompanyCode() {
        return companyCode;
    }

    public void setCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }

    public String getTwitterTweetsAnalyticPageSrc() {
        return twitterTweetsAnalyticPageSrc;
    }

    public void setTwitterTweetsAnalyticPageSrc(String twitterTweetsAnalyticPageSrc) {
        this.twitterTweetsAnalyticPageSrc = twitterTweetsAnalyticPageSrc;
    }

    public long getCurrentStartDate() {
        return currentStartDate;
    }

    public void setCurrentStartDate(long currentStartDate) {
        this.currentStartDate = currentStartDate;
        this.downloadsFolder = baseDownloadsFolder +  "/" + companyCode + "/" + currentStartDate;
        System.out.println("Twitter downloadsFolder 0 - " + this.downloadsFolder);
    }

    public long getCurrentEndDate() {
        return currentEndDate;
    }

    public void setCurrentEndDate(long currentEndDate) {
        this.currentEndDate = currentEndDate;
        if (currentEndDate > 0) {
            this.downloadsFolder = baseDownloadsFolder + "/" + companyCode + "/" + currentStartDate + "/" + currentEndDate;
            System.out.println("Twitter downloadsFolder 1 - " + this.downloadsFolder);
        }
        else {
            this.downloadsFolder = baseDownloadsFolder + "/" + companyCode + "/0/" + currentEndDate;
            System.out.println("Twitter downloadsFolder 2 - " + this.downloadsFolder);
        }
    }

    public void setUp() throws Exception {
        /*
        File file = new File(this.baseFolder + "/chromedriver");
        System.setProperty("webdriver.chrome.driver", file.getAbsolutePath());
        String downloadFilepath = downloadsFolder;
        HashMap<String, Object> chromePrefs = new HashMap<String, Object>();
        chromePrefs.put("profile.default_content_settings.popups", 0);
        chromePrefs.put("download.default_directory", downloadFilepath);
        ChromeOptions options = new ChromeOptions();
        options.setExperimentalOption("prefs", chromePrefs);
        DesiredCapabilities cap = DesiredCapabilities.chrome();
        cap.setCapability(CapabilityType.ACCEPT_SSL_CERTS, true);
        cap.setCapability(ChromeOptions.CAPABILITY, options);
        driver = new ChromeDriver(cap);
         */
        // Firefox
        System.setProperty("webdriver.gecko.driver", "/Users/pawan/git/github/pawank/sarkari-portal/drivers/geckodriver");
        driver = new FirefoxDriver();
        driver.manage().timeouts().implicitlyWait(30, TimeUnit.SECONDS);
    }


    /**
     */
    private void launchActivity() {
        driver.get(baseUrl + "/");
        //driver.manage().window().setSize(new Dimension(1600,900));
    }


    @SuppressWarnings("unused")
    private void maximizeWindow() throws InterruptedException {
        //driver.manage().window().maximize();
        driver.manage().window().setSize(new Dimension(1920, 1080));
        sleepShort();
    }


    @SuppressWarnings("unused")
    private void minimizeWindow() throws InterruptedException {
        driver.manage().window().setPosition(new Point(-6000, 0));
    }

    private void executeUrl(String url) throws InterruptedException {
        driver.get(url);
        sleepShort();
        String pageSrc = driver.getPageSource();
        if (pageSrc.toLowerCase().contains("We need some additional info to get started".toLowerCase())) {
            https://ads.twitter.com/user/nair_jagadeesh/tweets
            toElementByXpathAndSendKeysAndWait("/html/body/div[3]/div[1]/form/input", true);
            isFirsttime = false;
        }
    }

    private float getCorrectNoAsDecimal(String v) {
        float result = 0.0f;
        if ((v == null) || (v.isEmpty())) {
        } else {
            v = v.toLowerCase().replaceAll(",", "").replaceAll("%","");
            if (v.contains("k")) {
                String[] nos = v.split("k");
                result = Float.parseFloat(nos[0]) * 1000;
            } else if (v.contains("m")) {
                String[] nos = v.split("k");
                result = Float.parseFloat(nos[0]) * 1000000;
            } else {
                result = Float.parseFloat(v);
            }
        }
        return result;
    }

    private long getCorrectNo(String v) {
        return (long) getCorrectNoAsDecimal(v);
    }

    public String executeUrlAndExtractInfo(String url, String id) throws InterruptedException {
        driver.get(url);
        sleepShort();
        if ((id == null)  || (id.isEmpty())) {
            id = "read-more";
        }
        try {
        WebElement alink = driver.findElement(By.className("stayOnNext"));
        if (alink != null) {
            alink.click();
            Thread.sleep(500);
            String pageSrc = driver.getPageSource();
        }
    }catch(Exception ex) {
        ex.printStackTrace();
    }
        try {
        WebElement elem = driver.findElement(By.className(id));
        elem.click();
        Thread.sleep(1000);
        elem = driver.findElement(By.className("StyleContent"));
        //elem = driver.findElement(By.className("StyleContent  partial-description loaded open"));
        String text = elem.getText();
        if (text != null) {
            text = text.replace("Show Less","").replaceAll("\\s+", " ");
        }
        System.out.println("text - " + text);
        /*
        try {
            tearDown();
        }catch(Exception ex) {
            ex.printStackTrace();
        }
         */
        return text;
    }catch(Exception ex) {
        ex.printStackTrace();
        return "";
    }
    }

    public boolean isAnalyticsJsonSaved() {
        return isAnalyticsJsonSaved;
    }

    public void setAnalyticsJsonSaved(boolean isAnalyticsJsonSaved) {
        this.isAnalyticsJsonSaved = isAnalyticsJsonSaved;
    }

    public static void testing(String pageSrc) {
    }

    public List<String> getImpressionsEngagementAndEngagementRatePer28Days() {
        return this.impressionsEngagementAndEngagementRatePer28Days;
    }

    private void toElementByNameAndSendKeysAndWait(String elem, String keys) throws InterruptedException {
        int ok_size=driver.findElements(By.name(elem)).size();
        driver.findElements(By.name(elem)).get(ok_size - 1).click();
        //driver.findElement(By.name(elem)).click();
        driver.findElements(By.name(elem)).get(ok_size - 1).sendKeys(keys);
        sleepShort();
    }

    private void toElementByClassAndSendKeysAndWait(String elem, String keys) throws InterruptedException {
        int ok_size=driver.findElements(By.className(elem)).size();
        driver.findElements(By.className(elem)).get(ok_size - 1).click();
        //driver.findElement(By.name(elem)).click();
        driver.findElements(By.className(elem)).get(ok_size - 1).sendKeys(keys);
        sleepShort();
    }

    private void toElementByXpathAndSendKeysAndWait(String xpath, boolean isClick) throws InterruptedException {
        WebElement elem = driver.findElement(By.xpath(xpath));
        if (isClick)
            elem.click();
        sleepShort();
    }

    /**
     * Writes a string to an HTML element and waits a random time
     *
     * @param elem The element to send the keys
     * @param keys The string to write to the element
     * @throws InterruptedException Thrown when a thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity.
     */
    private void sendKeysAndWait(String elem, String keys) throws InterruptedException {
        driver.findElement(By.id(elem)).click();
        driver.findElement(By.id(elem)).sendKeys(keys);
        sleepShort();
    }


    /**
     * @param id The id of the button found in HTML
     * @throws InterruptedException Thrown when a thread is waiting, sleeping, or otherwise occupied, and the thread is interrupted, either before or during the activity.
     */
    private void clickButtonAndWait(@SuppressWarnings("SameParameterValue") String id) throws InterruptedException {
        driver.findElement(By.id(id)).click();
        sleepShort();
    }


    @SuppressWarnings("unused")
    private void clickCSSButtonAndWait(String id) throws InterruptedException {
        driver.findElement(By.cssSelector(id)).click();
        sleepShort();
    }


    private void clickXPathButtonAndWait(@SuppressWarnings("SameParameterValue") String id) throws InterruptedException {
        driver.findElement(By.xpath(id)).click();
        sleepShort();
    }


    private void clickClassButtonAndWait(@SuppressWarnings("SameParameterValue") String id) throws InterruptedException {
        driver.findElement(By.className(id)).click();
        sleepShort();
    }


    @SuppressWarnings("unused")
    private void sendDashedLinesStart() throws InterruptedException {
        clickButtonAndWait("global-new-tweet-button");
        sendKeysAndWait("tweet-box-global", "!----------");
        String selectAll = Keys.chord(Keys.CONTROL, Keys.RETURN);
        driver.findElement(By.id("tweet-box-global")).sendKeys(selectAll);
        sleepLong();
    }


    @SuppressWarnings("unused")
    private void sendDashedLinesEnd() throws InterruptedException {
        clickButtonAndWait("global-new-tweet-button");
        sendKeysAndWait("tweet-box-global", "----------!");
        String selectAll = Keys.chord(Keys.CONTROL, Keys.RETURN);
        driver.findElement(By.id("tweet-box-global")).sendKeys(selectAll);
        sleepLong();
    }


    private void sleepLong() throws InterruptedException {
        // Sends a tweet within a random interval of between 1 and 2 minutes
		/*int randomWaitDuration = 60000 + (int) (Math.random() * 120000);*/

        // Sends a tweet within a random interval of between 2.5 and 5 seconds
        int randomWaitDuration = 2500 + (int) (Math.random() * 5000);
        Thread.sleep(randomWaitDuration);
    }


    private void sleepShort() throws InterruptedException {
        int randomWaitDuration = 500 + (int) (Math.random() * 1000);
        Thread.sleep(randomWaitDuration);
    }


    public void tearDown() throws Exception {
        driver.quit();
        String verificationErrorString = verificationErrors.toString();
        if (!"".equals(verificationErrorString)) {
        }
    }

    public String getBaseDownloadsFolder() {
        return baseDownloadsFolder;
    }

    public void setBaseDownloadsFolder(String baseDownloadsFolder) {
        this.baseDownloadsFolder = baseDownloadsFolder;
    }

    public String getDownloadsFolder() {
        return downloadsFolder;
    }

    public void setDownloadsFolder(String downloadsFolder) {
        this.downloadsFolder = downloadsFolder;
    }
}
